use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::io::{Seek, SeekFrom};

use crate::core::op;
use crate::utils::{AsBytes, OutBuf};

struct LabelManager {
    label_map: HashMap<String, Option<u64>>,
    label_refs: Vec<(String, u64)>,
    func_map: HashMap<String, (u64, Option<u64>, usize)>,
    extrn_map: HashMap<String, u16>,
    const_map: HashMap<String, u16>,
    fcount: usize,
}

impl LabelManager {
    fn new() -> LabelManager {
        LabelManager {
            label_map: HashMap::new(),
            label_refs: Vec::new(),
            func_map: HashMap::new(),
            const_map: HashMap::new(),
            extrn_map: HashMap::new(),
            fcount: 0,
        }
    }

    fn put_func(&mut self, writer: &mut BufWriter<File>, flabel: &str) -> Result<(), String> {
        let cpos = writer.stream_position().map_err(|e| {
            format!("Failed to read file position at label {flabel} definition\n\tcaused by {e:?}")
        })?;
        self.func_map
            .insert(flabel.to_owned(), (cpos, None, self.fcount));
        self.fcount += 1;
        Ok(())
    }

    fn put_extern(&mut self, elabel: &str) {
        let len = self.extrn_map.len();
        self.extrn_map.insert(
            elabel.to_owned(),
            len.try_into().expect("At most 65535 extern declarations."),
        );
    }

    fn get_func_idx(&self, name: &str) -> Result<usize, String> {
        match self.func_map.get(name) {
            Some((_, _, idx)) => Ok(*idx),
            None => Err(format!("No function with label \'{name}\'")),
        }
    }

    fn get_extern_idx(&self, name: &str) -> Result<u16, String> {
        match self.extrn_map.get(name) {
            Some(idx) => Ok(*idx),
            None => Err(format!("No extern with label \'{name}\'")),
        }
    }

    fn def_label(&mut self, writer: &mut BufWriter<File>, label: &str) -> Result<(), String> {
        let cpos = writer.stream_position().map_err(|e| {
            format!("Failed to read file position at label {label} definition\n\tcaused by {e:?}")
        })?;

        if self.func_map.contains_key(label) {
            self.func_map.get_mut(label).unwrap().1 = Some(cpos);
            return Ok(());
        }

        if self.label_map.contains_key(label) {
            if self.label_map.get(label).unwrap().is_some() {
                return Err(format!(
                    "Label {label} already exists, and cannot be re-defined."
                ));
            }

            self.label_map.get_mut(label).unwrap().replace(cpos);
            return Ok(());
        }

        self.label_map.insert(label.to_owned(), Some(cpos));
        Ok(())
    }

    fn def_const_name(&mut self, name: &str, id: u16) {
        self.const_map.insert(name.to_owned(), id);
    }

    fn get_label(&mut self, writer: &mut BufWriter<File>, label: &str) -> Result<u32, String> {
        if self.label_map.contains_key(label) {
            let (k, v) = self.label_map.get_key_value(label).unwrap();
            if let Some(l) = v {
                Ok(*l as u32)
            } else {
                let cpos = writer.stream_position().map_err(|e| {
                    format!("Failed to read file position at label {label} ref\n\tcaused by {e:?}")
                })?;
                self.label_refs.push((k.to_string(), cpos));
                Ok(0)
            }
        } else {
            self.label_map.insert(label.to_owned(), None);
            let cpos = writer.stream_position().map_err(|e| {
                format!("Failed to read file position at label {label} ref\n\tcaused by {e:?}")
            })?;
            self.label_refs.push((label.to_owned(), cpos));
            Ok(0)
        }
    }
}

fn parse_intlit(lit: &str) -> Result<isize, String> {
    if let Some(stripped) = lit.strip_prefix("0x") {
        isize::from_str_radix(stripped, 16)
            .map_err(|e| format!("Failed to parse hex {},\n\tcaused by {:?}", lit, e))
    } else {
        lit.parse::<isize>()
            .map_err(|e| format!("Failed to parse decimal {},\n\tcaused by {:?}", lit, e))
    }
}

fn parse_fltlit(lit: &str) -> Result<f64, String> {
    str::parse(lit).map_err(|e| format!("Failed to parse float {}\n\tcaused  by {:?}", lit, e))
}

fn parse_reg(lit: &str, _: &LabelManager) -> Result<u8, String> {
    if let Some(stripped) = lit.strip_prefix('%') {
        let rv = parse_intlit(stripped)?;
        Ok((rv & 0xff) as u8)
    } else {
        Err(format!(
            "Expecting register value found {lit}, try prefixing with '%'",
        ))
    }
}

fn parse_constid(lit: &str, lman: &LabelManager) -> Result<u16, String> {
    if let Some(stripped) = lit.strip_prefix('$') {
        match lman.const_map.get(stripped) {
            Some(id) => Ok(*id),
            None => Err(format!("No such constant {lit}")),
        }
    } else {
        Ok(parse_intlit(lit)? as u16)
    }
}

pub fn _get_instruction_head(line: &str) -> Result<&str, String> {
    let Some(idx) = line.find(' ') else {
        return Err("Unrecognized syntax".to_string());
    };
    Ok(&line[0..idx])
}

// TODO: Remove `.collect()` everywhere, and replace with .next()

macro_rules! arg_instr {
	($end:expr, $line: ident, $lman: ident, $narg: literal, {$(($n: ident : $idx: literal, $func: ident)),*}) => {
		let vec: Vec<&str> = $line[$end..].split(", ").collect();
		if vec.len() < $narg {
			return Err(format!("Instruction expects {} parameters", $narg));
		}
		$(let $n = $func(vec[$idx], &$lman)?;)*
	};
}

pub fn asm(
    out: &mut BufWriter<File>,
    inf: &mut BufReader<File>,
    line_mut: &mut String,
    lno: &mut usize,
    emit_lines: bool,
) -> Result<(), String> {
    out.write_all(&crate::module::MAGIC)
        .map_err(|e| format!("Failed to write MAGIC into outfile,\n\tcaused by: {:?}", e))?;
    *lno = 1;
    let mut lman = LabelManager::new();
    let mut mode = 255; // 3 = Constant Pool, 1 = Func Decl, 2 = Extern, 0 = Code.
    let mut const_id = 0;
    let err_f = |e| format!("Failed Write.\n\tcaused by: {:?}", e);
    while let Ok(len) = inf.read_line(line_mut) {
        if len == 0 {
            break;
        }
        let line = line_mut.trim();
        if line.starts_with('#') || line.is_empty() {
            // Do nothing
        } else if line.starts_with(".cpool") {
            mode = 3;
            let pool_size = parse_intlit(&line[7..])?;
            out.write_u16((pool_size & 0xffff) as u16)
                .map_err(|e| format!("Failed Write\n\tcaused by: {:?}", e))?;
        } else if line.starts_with("f64") {
            if mode != 3 {
                return Err("Cannot specify constants outside constant pool.".to_owned());
            }
            let vec: Vec<&str> = line[4..].split(", ").collect();
            if vec.is_empty() {
                return Err("Missing parameters; literal value and optional name".to_owned());
            }
            let val = parse_fltlit(vec[0])?;
            if vec.len() == 2 {
                lman.def_const_name(&vec[1][1..], const_id);
            }
            out.write_u8(2).map_err(err_f)?; // Indicates const is float
            out.write_u64(val.to_bits()).map_err(err_f)?;
            const_id += 1;
        } else if line.starts_with("i64") {
            if mode != 3 {
                return Err("Cannot specify constants outside constant pool.".to_owned());
            }
            let vec: Vec<&str> = line[4..].split(", ").collect();
            if vec.is_empty() {
                return Err("Missing parameters; literal value and optional name".to_owned());
            }
            let val = parse_intlit(vec[0])? as u64;
            if vec.len() == 2 {
                lman.def_const_name(&vec[1][1..], const_id);
            }
            out.write_u8(1).map_err(err_f)?; // Indicates const is int
            out.write_u64(val).map_err(err_f)?;
            const_id += 1;
        } else if let Some(stripped) = line.strip_prefix("char") {
            if mode != 3 {
                return Err("Cannot specify constants outside constant pool.".to_owned());
            }
            let vec: Vec<&str> = stripped.split(", ").collect();
            if vec.is_empty() {
                return Err("Missing parameters; literal value and optional name".to_owned());
            }
            let val = line.chars().nth(5).unwrap() as u32;
            if vec.len() == 2 {
                lman.def_const_name(&vec[1][1..], const_id);
            }
            out.write_u8(3).map_err(err_f)?; // Indicates const is char
            out.write_u32(val).map_err(err_f)?;
            const_id += 1;
        } else if line.starts_with("str") {
            if mode != 3 {
                return Err("Cannot specify constants outside constant pool.".to_owned());
            }
            let st = line.find('"').map_or(4, |e| e + 1);
            let en = line.rfind('"').map_or(line.len(), |e| e - 1);
            let strtw = &line[st..=en];
            out.write_u8(4).map_err(err_f)?;
            out.write_u16(strtw.len() as u16).map_err(err_f)?;
            out.write_str(strtw).map_err(err_f)?;
            let vn = line.rfind('$');
            if let Some(i) = vn {
                if i > en {
                    lman.def_const_name(&line[(i + 1)..], const_id);
                }
            };
            const_id += 1;
        } else if line.starts_with(".extern") {
            mode = 2;
            if line.len() < 9 {
                return Err("Incomplete directive.".to_string());
            }
            let pool_size = parse_intlit(&line[8..])?;
            out.write_u16((pool_size & 0xffff) as u16).map_err(err_f)?;
        } else if line.starts_with(".ef") {
            if mode != 2 {
                return Err("Cannot declare external reference outside extern".to_owned());
            }
            out.write_u8(0).map_err(err_f)?; // This is an extern, not a native function.
            let end = line
                .rfind(',')
                .ok_or("Incomplete extern declaration. Extern label missing.")?;
            let path = &line[4..end];
            out.write_u16(
                path.len()
                    .try_into()
                    .expect("Path length cannot exceed 65535"),
            )
            .map_err(err_f)?;
            out.write_str(path).map_err(err_f)?;
            let end = line
                .rfind('@')
                .ok_or("Label is mandatory for extern declarations.")?;
            let name = &line[(end + 1)..];
            lman.put_extern(name);
        } else if line.starts_with(".nf") {
            if mode != 2 {
                return Err("Cannot declare external reference outside extern".to_owned());
            }
            out.write_u8(1).map_err(err_f)?; // This is an extern, not a native function.
            let end = line
                .rfind(',')
                .ok_or("Incomplete extern declaration. Extern label missing.")?;
            let libn = &line[4..end].trim();
            out.write_u16(
                libn.len()
                    .try_into()
                    .expect("Library identifier cannot exceed 65535"),
            )
            .map_err(err_f)?;
            out.write_str(libn).map_err(err_f)?;
            let end = line
                .rfind('@')
                .ok_or("Label is mandatory for extern declarations.")?;
            let name = &line[(end + 1)..];
            lman.put_extern(name);
        } else if line.starts_with(".fdecl") {
            mode = 1;
            if line.len() < 8 {
                return Err("Incomplete directive.".to_string());
            }
            let pool_size = parse_intlit(&line[7..])?;
            out.write_u16((pool_size & 0xffff) as u16).map_err(err_f)?;
        } else if line.starts_with(".df") {
            if mode != 1 {
                return Err("Cannot declare function outside fdecl".to_owned());
            }
            let vec: Vec<&str> = line[4..].split(", ").collect();
            out.write_u8(vec[0].len() as u8).map_err(err_f)?;
            out.write_str(vec[0]).map_err(err_f)?;
            out.write_u8(parse_intlit(vec[1])? as u8).map_err(err_f)?;
            out.write_u8(parse_intlit(vec[2])? as u8).map_err(err_f)?;
            //dbg!(&line,&vec);
            if vec[3].starts_with('@') {
                lman.put_func(out, &vec[3][1..])?;
                out.write_u32(0).map_err(err_f)?;
            } else {
                return Err("Cannot declare function without label to code block.".to_owned());
            }
        } else if line.starts_with(".code") {
            mode = 0;
        } else if line.ends_with(':') {
            let fst = line.split(' ').next().unwrap().replace(':', "");
            lman.def_label(out, &fst)?;
        } else if line.starts_with(".clr") {
            //dbg!(&lman.label_map, &lman.label_refs);
            let start_off = out
                .stream_position()
                .map_err(|e| format!("Failed to set up restoring offset\n\tcaused by {:?}", e))?;
            for (lname, write_off) in &lman.label_refs {
                out.seek(SeekFrom::Start(*write_off)).map_err(err_f)?;
                if let Some(off) = lman.label_map.get(lname).unwrap() {
                    out.write_u32(*off as u32).map_err(err_f)?;
                } else {
                    return Err(format!("Label {} is referenced but never defined.", lname));
                }
            }
            out.seek(SeekFrom::Start(start_off)).map_err(err_f)?;
            lman.label_map.clear();
            lman.label_refs.clear();
        } else if line.starts_with("ret") {
            if line.len() < 4 {
                out.write_u8(op::VRET).map_err(err_f)?;
            } else {
                let r1 = parse_reg(&line[4..], &lman)?;
                out.write_u8(op::RET).map_err(err_f)?;
                out.write_u8(r1).map_err(err_f)?;
            }
        } else if line.starts_with("ldc") {
            out.write_u8(op::LDC).map_err(err_f)?;
            let vec: Vec<&str> = line[4..].split(", ").collect();
            let id = parse_constid(vec[0], &lman)?;
            let r1 = parse_reg(vec[1], &lman)?;
            let instr = op::Id16Reg { id, r1 };
            instr.write(out).map_err(err_f)?;
        } else if line.starts_with("ldi") {
            out.write_u8(op::LDI).map_err(err_f)?;
            let vec: Vec<&str> = line[4..].split(", ").collect();
            let val = parse_intlit(vec[0])? as i8;
            let r1 = parse_reg(vec[1], &lman)?;
            out.write_u8(val as u8).map_err(err_f)?;
            out.write_u8(r1).map_err(err_f)?;
        } else if line.starts_with("ldx") {
            out.write_u8(op::LDX).map_err(err_f)?;
            let vec: Vec<&str> = line[4..].split(", ").collect();
            if !vec[0].starts_with('@') {
                return Err("Cannot load extern without appropriate label.".to_owned());
            }
            let id = lman.get_extern_idx(&vec[0][1..])?;
            let r1 = parse_reg(vec[1], &lman)?;
            let instr = op::Id16Reg { id, r1 };
            instr.write(out).map_err(err_f)?;
        } else if line.starts_with("ldf") {
            out.write_u8(op::LDF).map_err(err_f)?;
            let vec: Vec<&str> = line[4..].split(", ").collect();
            if !vec[0].starts_with('@') {
                return Err("Cannot load function without appropriate label.".to_owned());
            }
            let r1 = parse_reg(vec[1], &lman)?;
            let id = lman.get_func_idx(&vec[0][1..])? as u16;
            let instr = op::Id16Reg { id, r1 };
            instr.write(out).map_err(err_f)?;
        } else if line.starts_with("stdcall") {
            let args: Vec<&str> = line[8..].split(", ").collect();
            let r1 = parse_reg(args[0], &lman)?;
            let r2 = parse_reg(args[1], &lman)?;
            let mut rvec = vec![0; args.len() - 2];
            for i in 0..rvec.len() {
                rvec[i] = parse_reg(args[i + 2], &lman)?;
            }
            out.write_u8(op::STDCALL).map_err(err_f)?;
            let instr = op::DoubleRegst { r1, r2 };
            instr.write(out).map_err(err_f)?;
            let instr = op::VariadicRegst { regs: rvec };
            instr.write(out).map_err(err_f)?;
        } else if line.starts_with("inc") {
            arg_instr!(4, line, lman, 2, {(r1: 0, parse_reg), (r2: 1, parse_constid)});
            if r2 == 1 {
                out.write_u8(op::INC1).map_err(err_f)?;
                out.write_u8(r1).map_err(err_f)?;
            } else {
                out.write_u8(op::IINC).map_err(err_f)?;
                let instr = op::Id16Reg { r1, id: r2 };
                instr.write(out).map_err(err_f)?;
            }
        } else if line.starts_with("jmp") {
            if line.len() < 6 {
                return Err("Incomplete instruction. Jump destination missing.".to_string());
            }
            let label = &line[5..];
            if line.chars().nth(4).unwrap() != '@' {
                return Err("Jump to unlabelled code point is not allowed.".to_string());
            }
            out.write_u8(op::JUMP).map_err(err_f)?;
            let res = lman.get_label(out, label)?;
            out.write_u32(res).map_err(err_f)?;
        } else if line.starts_with("branch") {
            let args: Vec<&str> = line[7..].split(", ").collect();
            let r1 = parse_reg(args[0], &lman)?;
            if !args[1].starts_with('@') {
                return Err("Branch to unlabelled code point is not allowed.".to_string());
            }
            out.write_u8(op::BRANCH).map_err(err_f)?;
            out.write_u8(r1).map_err(err_f)?;
            let res = lman.get_label(out, &args[1][1..])?;
            out.write_u32(res).map_err(err_f)?;
        } else if line.starts_with("slice") {
            arg_instr!(6, line, lman, 4, {(r1: 0, parse_reg), (r2: 1, parse_reg), (s1: 2, parse_reg), (s2: 3, parse_reg)});
            out.write_u8(op::SLICE).map_err(err_f)?;
            op::QuadrupleRegst { r1, r2, s1, s2 }
                .write(out)
                .map_err(err_f)?;
        } else if line.starts_with("nrange") {
            arg_instr!(7, line, lman, 4, {(r1: 0, parse_reg), (r2: 1, parse_reg), (s1: 2, parse_reg), (s2: 3, parse_reg)});
            out.write_u8(op::NEWRANGE).map_err(err_f)?;
            op::QuadrupleRegst { r1, r2, s1, s2 }
                .write(out)
                .map_err(err_f)?;
        } else if line.starts_with("popor") {
            let args: Vec<&str> = line[6..].split(", ").collect();
            let r1 = parse_reg(args[0], &lman)?;
            let r2 = parse_reg(args[1], &lman)?;
            if !args[2].starts_with('@') {
                return Err("Branch to unlabelled code point is not allowed.".to_string());
            }
            let res = lman.get_label(out, &args[2][1..])?;
            out.write_u8(op::POPOR).map_err(err_f)?;
            out.write_u8(r1).map_err(err_f)?;
            out.write_u8(r2).map_err(err_f)?;
            out.write_u32(res).map_err(err_f)?;
        } else if line.starts_with("nop") {
            out.write_u8(op::NOP).map_err(err_f)?;
        } else {
            let head = _get_instruction_head(line)?;
            let off = head.len() + 1;
            if let Some(opc) = op::triplet_mnemonic_map(head) {
                arg_instr!(off, line, lman, 3, {(r1: 0, parse_reg), (r2: 1, parse_reg), (r3: 2, parse_reg)});
                out.write_u8(opc).map_err(err_f)?;
                let instr = op::TripleRegst { r1, r2, r3 };
                instr.write(out).map_err(err_f)?;
            } else if let Some(opc) = op::doublet_mnemonic_map(head) {
                arg_instr!(off, line, lman, 2, {(r1: 0, parse_reg), (r2: 1, parse_reg)});
                out.write_u8(opc).map_err(err_f)?;
                op::DoubleRegst { r1, r2 }.write(out).map_err(err_f)?;
            } else if let Some(opc) = op::singlet_mnemonic_map(head) {
                let r1 = parse_reg(&line[off..], &lman)?;
                out.write_u8(opc).map_err(err_f)?;
                out.write_u8(r1).map_err(err_f)?;
            } else {
                return Err("Unrecognized syntax.".to_string());
            }
        }
        *lno += 1;
        if emit_lines && mode == 0 {
            out.write_u8(op::DBGLN).map_err(err_f)?;
            out.write_u32(*lno as u32).map_err(err_f)?;
        }
        line_mut.clear();
    }

    // Resolve all function labels.
    for (name, offinf) in lman.func_map {
        let (write_off, pos, _idx) = offinf;
        out.seek(SeekFrom::Start(write_off)).map_err(err_f)?;
        match pos {
            Some(u) => {
                out.write_u32(u as u32).map_err(err_f)?;
            }
            None => {
                return Err(format!("Label {name} not defined."));
            }
        }
    }

    Ok(())
}
