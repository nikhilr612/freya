use core::time::Duration;
use std::io::BufRead;
use std::str::FromStr;
use std::sync::{Arc, RwLock};

use crate::args::RefPolicy;
use crate::core::module::{ModulePool, PoolWriteGuard, ResolutionStatus};
use crate::core::types::{self, new_error, BaseType, ErrorType};
use crate::core::{op, FResult};
use crate::native::InterfacePool;

mod frame;
mod rc;

use frame::CallFrame;
pub use rc::RefCounter;

/// A VM thread/worker which fetch-decode-executes instructions from an entry point function, whose frame is first pushed onto stack.
/// Note that this does not actually correspond to an OS-level thread.
pub struct WorkerThread {
    /// Reference counter to keep track of number of active references during execution.
    refc: RefCounter,
    /// Stack to store current execution state and function information.
    stack: Vec<CallFrame>,
    /// Pool to load all modules required at runtime.
    /// Provides read-only access to modules.
    /// Pool is locked when a module is being loaded.
    pool: Arc<ModulePool>,
    nifp: Arc<RwLock<InterfacePool>>,
}

impl WorkerThread {
    /// Create a new WorkerThread for module `mpath` with entry point `ep`.
    #[allow(dead_code)]
    pub fn new(
        mpath: &str,
        ep: &str,
        refp: RefPolicy,
        mp: Arc<ModulePool>,
        nifp: Arc<RwLock<InterfacePool>>,
    ) -> WorkerThread {
        let (cf, _) = CallFrame::from_fdecl(&mp, mpath, ep);
        let stack = vec![cf]; // Load main onto stack.
        WorkerThread {
            refc: RefCounter::new(refp),
            stack,
            pool: mp,
            nifp,
        }
    }

    /// Call a resolved callable in register `callr` with parameters `param` (possible in tail call manner if `is_tc`)
    fn _common_call(
        &mut self,
        callr: u8,
        param: Vec<Option<BaseType>>,
        is_tc: bool,
    ) -> FResult<()> {
        let val = self
            .stack
            .last()
            .expect("Ultimate cockup. Common call initiated with empty stack")
            .read_register(callr)?;
        let (mod_id, func_id, native) = _extract_finfo(val)?;

        if native {
            todo!("Implement native function calls.")
        }

        let new_frame = _make_frame(param, &self.pool, mod_id, func_id)?;

        if is_tc {
            let idx = self.stack.len() - 1;
            let old_frame = std::mem::replace(&mut self.stack[idx], new_frame); // Replace the top-most frame in case of tail-call,
            old_frame.release(&mut self.refc)? // and release the old frame
        } else {
            self.stack.push(new_frame); // Push a new frame in the normal case.
        }

        Ok(())
    }

    /// Create a new WorkerThread for module `mpath` with entry point `ep`.
    pub fn with_args<T>(
        mpath: &str,
        ep: &str,
        refp: RefPolicy,
        mp: Arc<ModulePool>,
        nifp: Arc<RwLock<InterfacePool>>,
        args: T,
    ) -> WorkerThread
    where
        T: Iterator<Item = BaseType>,
    {
        let (mut cf, n) = CallFrame::from_fdecl(&mp, mpath, ep);
        for (i, a) in args.take(n).enumerate() {
            cf.regs[i].replace(a);
        }
        let stack = vec![cf]; // Load main onto stack.
        WorkerThread {
            refc: RefCounter::new(refp),
            stack,
            pool: mp,
            nifp,
        }
    }

    pub fn print_stack_trace(&self) {
        eprintln!("Trace back (most recent call last)");
        let mvec = self.pool.read_lock();
        for frame in &self.stack {
            let fmod = &mvec[frame.module_id];
            let fname = fmod.name_by_id(frame.function_id()).unwrap();
            let mname = &fmod.name;
            let debug_lnum = frame.lineno();
            if debug_lnum != 0 {
                eprintln!("\t@{fname}\t[{}:{:#06x}], {mname}", debug_lnum, frame.ip);
            } else {
                eprintln!("\t@{fname}\t[{:#06x}], {mname}", frame.ip)
            }
        }
    }
}

fn resolve_extern(
    pool: &ModulePool,
    mut mvec: PoolWriteGuard,
    ext_ids: (usize, usize),
) -> FResult<(usize, usize, bool)> {
    let ext = mvec[ext_ids.0].extern_ref(ext_ids.1);
    match ext.status {
        ResolutionStatus::Resolved => {
            // We've already done this.
            Ok((ext.module_id, ext.function_id, ext.native)) // A-OK.
        }
        ResolutionStatus::Unresolved if !ext.native => {
            // Unresolved non-native extern.
            let (module_id, function_id) = match pool.id_by_name(&ext.module_path) {
                Ok(module_id) => {
                    // if module is loaded retrieve function id
                    let function_id = mvec[module_id].func_id(&ext.func_name)?;
                    (module_id, function_id)
                }
                Err(_) => {
                    // otherwise load it.
                    let pbuf = pool.resolve_path(&ext.module_path)?;
                    let module = crate::module::open(pbuf.to_str().unwrap())?;
                    let function_id = module.func_id(&ext.func_name)?;
                    let module_id = mvec.len();
                    pool.update_path(&ext.module_path, module_id);
                    mvec.push(module);
                    (module_id, function_id)
                }
            };
            let ext = mvec[ext_ids.0].extern_mutref(ext_ids.1);
            // Update extern, and change to resolved.
            ext.module_id = module_id;
            ext.function_id = function_id;
            ext.status = ResolutionStatus::Resolved;
            Ok((module_id, function_id, false))
        }
        ResolutionStatus::Unresolved => {
            todo!("Native functions have not been implemented yet.");
        }
    }
}

#[inline]
/// A call is deemed a 'tail-call' if:
/// -    it is immediately followed by a `VRET` (in all cases)
/// -    it is immediately followed by a `return <current return register>` (if call doesn't discard return value)
// TODO: Recheck Tail-call implementation.
fn _check_tailcall(slvw: &mut crate::utils::SliceView, rslot: Option<u8>) -> bool {
    let nextinstr = slvw.get_u8();
    if nextinstr == op::VRET {
        return true;
    }
    if nextinstr == op::RET {
        let reg_id = slvw.get_u8();
        if let Some(c_reg_id) = rslot {
            return reg_id == c_reg_id;
        }
    }
    false
}

// RN this method literally checks if value is FRef and copies its contents
fn _extract_finfo(val: &BaseType) -> FResult<(usize, usize, bool)> {
    if let BaseType::Alloc(cpt) = val {
        if let types::CompositeType::FRef {
            mod_id,
            func_idx,
            native,
        } = &**cpt
        {
            Ok((*mod_id, *func_idx, *native))
        } else {
            Err(new_error(
                ErrorType::NotCallable,
                format!("Incompatible value. {val:?} is not a callable."),
            ))
        }
    } else {
        Err(new_error(
            ErrorType::NotCallable,
            format!("Incompatible value. {val:?} is not a callable."),
        ))
    }
}

fn _make_frame(
    param: Vec<Option<BaseType>>,
    pool: &ModulePool,
    mid: usize,
    fid: usize,
) -> FResult<CallFrame> {
    let (mut new_frame, narg) = CallFrame::from_fnid(pool, mid, fid);
    if param.len() != narg {
        let mvec = pool.read_lock();
        let name = mvec[mid].name_by_id(fid).unwrap();
        return Err(new_error(
            ErrorType::InvalidCall,
            format!(
                "Function {name} requires {narg} parameters, but {} arguments were supplied.",
                param.len()
            ),
        ));
    }
    // Put args.
    for (i, pval) in param.into_iter().enumerate() {
        new_frame.regs[i] = pval;
    }
    Ok(new_frame)
}

#[inline]
fn _get_line(prompt: &str) -> String {
    print!("{prompt}");
    let stdin = std::io::stdin();
    stdin.lock().lines().next().unwrap().unwrap()
}

macro_rules! instr_3arg {
    ($func: expr, $slvw: ident, $frame: ident, $refc: expr) => {{
        let param = op::TripleRegst::try_from(&mut $slvw)?;

        let rv1 = $frame.read_register(param.r1)?;
        let rv2 = $frame.read_register(param.r2)?;
        let rid = param.r3;

        let res = $func(rv1, rv2)?;
        $frame.write_register(rid, res, &mut $refc)?;
    }};
}

macro_rules! instr_2arg {
    ($slvw: ident, $frame: ident, $refc: expr, |$val:ident| $body: block) => {{
        let param = op::DoubleRegst::try_from(&mut $slvw)?;
        let $val = $frame.read_register(param.r1)?;
        let res = $body;
        $frame.write_register(param.r2, res, &mut $refc)?;
    }};
}

impl WorkerThread {
    /// Begin execution.
    pub fn begin(&mut self) -> FResult<()> {
        while !self.stack.is_empty() {
            let stack_top = self.stack.len() - 1;
            let cur_frame = &mut self.stack[stack_top]; // can be moved out
            let module_pool_vec_read = self.pool.read_lock(); // --> this HAS to stay here.
            let cur_module = &module_pool_vec_read[cur_frame.module_id]; // so this can't be outside loop either
            let mut slvw = cur_module.new_view(cur_frame.ip); // this also, ig ; but it doesn't really matter
            let opcode = slvw.get_u8();
            // Ugly match arm. Could do better.
            match opcode {
                op::VRET => {
                    self.stack
                        .pop()
                        .expect("Cannot pop empty call stack.\n\tComment: Ultimate cockup.")
                        .release(&mut self.refc)?; // This should never panic.
                    continue;
                }
                op::RET => {
                    let r1 = slvw.get_u8();
                    let mut frame = self
                        .stack
                        .pop()
                        .expect("Cannot pop empty call stack.\n\tComment: Ultimate cockup.");
                    if !self.stack.is_empty() {
                        let cur_frame = &mut self.stack[stack_top - 1];
                        if let Some(ret_reg) = cur_frame.rslot {
                            let val = frame.take_register(r1, &mut self.refc)?;
                            cur_frame.writeopt_register(ret_reg, val, &mut self.refc)?;
                            cur_frame.rslot = None;
                        }
                    }
                    frame.release(&mut self.refc)?;
                    continue;
                }
                op::LDC => {
                    let itype = op::Id16Reg::try_from(&mut slvw)?;
                    let id = itype.id as usize;
                    let mrfc = &mut self.refc;
                    let val = cur_module.clone_constant(id, mrfc).unwrap();
                    cur_frame.write_register(itype.r1, val, mrfc)?;
                }
                op::PRINT => {
                    let rval = cur_frame.read_register(slvw.get_u8())?;
                    println!("{}", types::as_printrepr(rval)?);
                }
                op::SLEEP => {
                    let rv1 = cur_frame.read_register(slvw.get_u8())?;
                    let dur = match rv1 {
                        BaseType::Int(i) => Duration::from_secs(*i as u64),
                        BaseType::Flt(f) => Duration::from_micros(f64::round(*f * 1e6) as u64),
                        _ => {
                            return Err(types::new_error(ErrorType::TypeMismatch, format!("Sleep requires NUMERIC type. Only Flt or Int. Cannot infer duration from {rv1}")));
                        }
                    };
                    std::thread::sleep(dur);
                }
                op::ADD => instr_3arg!(types::try_add, slvw, cur_frame, self.refc),
                op::SUB => instr_3arg!(types::try_sub, slvw, cur_frame, self.refc),
                op::MUL => instr_3arg!(types::try_mul, slvw, cur_frame, self.refc),
                op::EXP => instr_3arg!(types::try_exp, slvw, cur_frame, self.refc),
                op::DIV => instr_3arg!(types::try_div, slvw, cur_frame, self.refc),
                op::IDIV => instr_3arg!(types::try_idiv, slvw, cur_frame, self.refc),
                op::REM => instr_3arg!(types::try_rem, slvw, cur_frame, self.refc),
                op::BAND => instr_3arg!(types::try_bitwise_and, slvw, cur_frame, self.refc),
                op::BOR => instr_3arg!(types::try_bitwise_or, slvw, cur_frame, self.refc),
                op::XOR => instr_3arg!(types::try_bitwise_xor, slvw, cur_frame, self.refc),
                op::LSHIFT => instr_3arg!(types::try_left_bitshift, slvw, cur_frame, self.refc),
                op::RSHIFT => instr_3arg!(types::try_right_bitshift, slvw, cur_frame, self.refc),
                op::RLT => instr_3arg!(types::try_lt, slvw, cur_frame, self.refc),
                op::RLE => instr_3arg!(types::try_lte, slvw, cur_frame, self.refc),
                op::RGT => instr_3arg!(types::try_gt, slvw, cur_frame, self.refc),
                op::RGE => instr_3arg!(types::try_gte, slvw, cur_frame, self.refc),
                op::REQ => instr_3arg!(types::try_eq, slvw, cur_frame, self.refc),
                op::RNEQ => instr_3arg!(types::try_neq, slvw, cur_frame, self.refc),
                op::LDI => {
                    let val = slvw.get_u8() as i8;
                    let out_reg = slvw.get_u8();
                    let val = types::BaseType::Int(val as i64);
                    cur_frame.write_register(out_reg, val, &mut self.refc)?;
                }
                op::SWAP => {
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    let r1 = dreg.r1 as usize % cur_frame.regs.len();
                    let r2 = dreg.r2 as usize % cur_frame.regs.len();
                    cur_frame.regs.swap(r1, r2);
                }
                op::IINC => {
                    let param = op::Id16Reg::try_from(&mut slvw)?;
                    let rv1 = cur_frame.read_register(param.r1)?;
                    let rv2 = types::BaseType::Int((param.id as i16) as i64);
                    let res = types::try_add(rv1, &rv2)?;
                    cur_frame.write_register(param.r1, res, &mut self.refc)?;
                }
                op::INC1 => {
                    let r1 = slvw.get_u8();
                    let rv1 = cur_frame.read_register(r1)?;
                    let rv2 = types::BaseType::Int(1);
                    let res = types::try_add(rv1, &rv2)?;
                    cur_frame.write_register(r1, res, &mut self.refc)?;
                }
                op::MOV => {
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    cur_frame.tmove_register(dreg.r1, dreg.r2, &mut self.refc)?;
                }
                op::DROP => {
                    let r1 = slvw.get_u8();
                    cur_frame.drop_register(r1, &mut self.refc)?;
                }
                op::DEL => {
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    for r in dreg.r1..=dreg.r2 {
                        cur_frame.drop_register(r, &mut self.refc)?;
                    }
                }
                op::LDF => {
                    let param = op::Id16Reg::try_from(&mut slvw)?;
                    let val = types::CompositeType::new_fref(
                        cur_frame.module_id,
                        param.id as usize,
                        false,
                    );
                    cur_frame.write_register(param.r1, val, &mut self.refc)?;
                }
                op::LDX => {
                    let param = op::Id16Reg::try_from(&mut slvw)?;
                    cur_frame.ip = slvw.offset();
                    std::mem::drop(module_pool_vec_read);
                    let val = {
                        let mvec = self.pool.write_lock();
                        resolve_extern(&self.pool, mvec, (cur_frame.module_id, param.id as usize))
                    }?;
                    cur_frame.write_register(
                        param.r1,
                        types::CompositeType::new_fref(val.0, val.1, val.2),
                        &mut self.refc,
                    )?;
                    continue;
                }
                op::LNOT => {
                    let r1 = slvw.get_u8();
                    let rv1 = cur_frame.read_register(r1)?;
                    let res = types::try_lnot(rv1)?;
                    cur_frame.write_register(r1, res, &mut self.refc)?;
                }
                op::JUMP => {
                    let addr = slvw.get_u32();
                    cur_frame.ip = addr as usize;
                    continue;
                }
                op::BRANCH => {
                    let param = op::RegAddr::try_from(&mut slvw)?;
                    let rv = cur_frame.read_register(param.r1)?;
                    if !types::as_bool(rv) {
                        cur_frame.ip = param.addr as usize;
                        continue;
                    }
                }
                op::STDCALL => {
                    // Read instruction
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    cur_frame.rslot = if dreg.r2 != 0 {
                        Some(dreg.r2 - 1)
                    } else {
                        None
                    };
                    let rparam = op::VariadicRegst::try_from(&mut slvw)?;

                    // Set ip to next instr, for execution after return
                    cur_frame.ip = slvw.offset();
                    let is_tc = _check_tailcall(&mut slvw, cur_frame.rslot); // Also check if TCO is possible.

                    // Copy/Move arguments
                    let param: FResult<Vec<_>> = rparam
                        .regs
                        .into_iter()
                        .map(|r| cur_frame.take_register(r, &mut self.refc))
                        .collect();

                    // Drop this guard here; along with cur_frame, etc.
                    std::mem::drop(module_pool_vec_read);

                    // Read FRef, they are already resolved now.
                    self._common_call(dreg.r1, param?, is_tc)?;

                    continue;
                }
                op::FSTCALL => {
                    let qreg = op::QuadrupleRegst::try_from(&mut slvw)?;
                    cur_frame.rslot = if qreg.r2 != 0 {
                        Some(qreg.r2 - 1)
                    } else {
                        None
                    };

                    // Set ip to next instr, for execution after return.
                    cur_frame.ip = slvw.offset();
                    let is_tc = _check_tailcall(&mut slvw, cur_frame.rslot); // Check if TCO is possible.

                    // Prepare arguments.
                    let param: FResult<Vec<_>> = (qreg.s1..(qreg.s1 + qreg.s2))
                        .map(|rid| cur_frame.take_register(rid, &mut self.refc))
                        .collect();

                    // Drop this guard here; along with cur_frame, etc.
                    std::mem::drop(module_pool_vec_read);

                    // Read FRef, they are already resolved now.
                    self._common_call(qreg.r1, param?, is_tc)?;

                    continue;
                }
                op::BWI => {
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    let val = cur_frame.read_register(dreg.r1)?;
                    let cref = match val {
                        BaseType::Alloc(v) => v.as_ref() as *const types::CompositeType,
                        _ => {
                            return Err(new_error(
                                ErrorType::TypeMismatch,
                                format!("Cannot borrow {val}"),
                            ));
                        }
                    };
                    self.refc.incref(cref)?;
                    cur_frame.write_register(dreg.r2, BaseType::ConstRef(cref), &mut self.refc)?;
                }
                op::BWM => {
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    let val = cur_frame.read_mutregst(dreg.r1)?;
                    let mref = match val {
                        BaseType::Alloc(v) => v.as_mut() as *mut types::CompositeType,
                        _ => {
                            return Err(new_error(
                                ErrorType::TypeMismatch,
                                format!("Cannot borrow {val}"),
                            ));
                        }
                    };
                    self.refc.incref_mut(mref)?;
                    cur_frame.write_register(dreg.r2, BaseType::MutRef(mref), &mut self.refc)?;
                }
                op::NOP => {}
                op::DBGLN => {
                    let line_number = slvw.get_u16();
                    cur_frame.set_lineno(line_number);
                }
                op::ASSERT => {
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    let val = cur_frame.read_register(dreg.r1)?;
                    if !types::as_bool(val) {
                        if usize::from(dreg.r2) >= cur_frame.regs.len() {
                            return Err(new_error(
                                ErrorType::AssertionFailure,
                                format!("Value {val} evaluates to false"),
                            ));
                        } else {
                            let msg = cur_frame.read_register(dreg.r2)?;
                            return Err(new_error(ErrorType::AssertionFailure, format!("{msg}")));
                        }
                    }
                }
                op::PARSEINT => instr_2arg!(slvw, cur_frame, self.refc, |val| {
                    let text = match types::as_composite_type(val, &self.refc)? {
                        types::CompositeType::Str(s) => s,
                        _ => {
                            return Err(new_error(
                                ErrorType::TypeMismatch,
                                format!("Cannot parse integer from {val} (not Str)"),
                            ));
                        }
                    };
                    BaseType::Int(i64::from_str(text).map_err(|_| {
                        new_error(
                            ErrorType::ParsingFailure,
                            format!("Could not parse \"{text}\" as Int"),
                        )
                    })?)
                }),
                op::PARSEFLT => instr_2arg!(slvw, cur_frame, self.refc, |val| {
                    let text = match types::as_composite_type(val, &self.refc)? {
                        types::CompositeType::Str(s) => s,
                        _ => {
                            return Err(new_error(
                                ErrorType::TypeMismatch,
                                format!("Cannot parse float from {val} (not Str)"),
                            ));
                        }
                    };
                    BaseType::Int(i64::from_str(text).map_err(|_| {
                        new_error(
                            ErrorType::ParsingFailure,
                            format!("Could not parse \"{text}\" as Flt"),
                        )
                    })?)
                }),
                op::GETLN => instr_2arg!(slvw, cur_frame, self.refc, |val| {
                    BaseType::Alloc(Box::new(types::CompositeType::Str(_get_line(
                        &types::as_printrepr(val)?,
                    ))))
                }),
                op::LENGTH => instr_2arg!(slvw, cur_frame, self.refc, |val| {
                    BaseType::Int((types::as_composite_type(val, &self.refc)?.length()?) as i64)
                }),
                op::BNOT => instr_2arg!(slvw, cur_frame, self.refc, |rval| {
                    match rval {
                        BaseType::Int(i1) => BaseType::Int(!*i1),
                        _ => {
                            return Err(types::new_error(
                                ErrorType::TypeMismatch,
                                format!("Unsupported bitwise not on {rval}"),
                            ));
                        }
                    }
                }),
                op::FLOOR => instr_2arg!(slvw, cur_frame, self.refc, |rval| {
                    types::try_floor(rval).ok_or(types::new_error(
                        ErrorType::TypeMismatch,
                        format!("Unsupported 'floor' on {rval}"),
                    ))?
                }),
                op::ISN0 => instr_2arg!(slvw, cur_frame, self.refc, |rv| {
                    if types::as_bool(rv) {
                        types::BaseType::Int(1)
                    } else {
                        types::BaseType::Int(0)
                    }
                }),
                op::FSLICE => instr_2arg!(slvw, cur_frame, self.refc, |rv| {
                    types::as_composite_type(rv, &self.refc)?.full_slice(&mut self.refc)?
                }),
                op::REVERSE => {
                    let r1 = slvw.get_u8();
                    let rval = cur_frame.read_mutregst(r1)?;
                    types::as_composite_type_mut(rval, &self.refc)?.reverse_in_place()?
                }
                op::PUSH => {
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    let pval =
                        cur_frame
                            .take_register(dreg.r2, &mut self.refc)?
                            .ok_or(new_error(
                                ErrorType::EmptyRegister,
                                format!("Cannot push value from empty register {}", dreg.r2),
                            ))?;
                    let rval = cur_frame.read_mutregst(dreg.r1)?;
                    types::as_composite_type_mut(rval, &self.refc)?.push(pval)?;
                }
                op::POP => {
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    let rval = cur_frame.read_mutregst(dreg.r1)?;
                    let wval =
                        types::as_composite_type_mut(rval, &self.refc)?.pop(&mut self.refc)?;
                    cur_frame.write_register(dreg.r2, wval, &mut self.refc)?;
                }
                op::POPOR => {
                    let breg = op::BiRegAddr::try_from(&mut slvw)?;
                    let rval = types::as_composite_type_mut(
                        cur_frame.read_mutregst(breg.r1)?,
                        &self.refc,
                    )?;
                    if let Ok(v) = rval.pop(&mut self.refc) {
                        cur_frame.write_register(breg.r2, v, &mut self.refc)?;
                    } else {
                        cur_frame.ip = breg.addr as usize;
                    }
                }
                op::SLICE => {
                    let qreg = op::QuadrupleRegst::try_from(&mut slvw)?;
                    let ctype =
                        types::as_composite_type(cur_frame.read_register(qreg.r2)?, &self.refc)?;
                    let s1 = cur_frame.read_register(qreg.s1)?;
                    let s2 = cur_frame.read_register(qreg.s2)?;
                    let val = ctype.slice(s1, s2, &mut self.refc)?;
                    cur_frame.write_register(qreg.r1, val, &mut self.refc)?;
                }
                op::GETINDEX => {
                    let treg = op::TripleRegst::try_from(&mut slvw)?;
                    let rv1 = cur_frame.read_register(treg.r1)?;
                    let rvi = cur_frame.read_register(treg.r2)?;
                    let val = types::try_index(rv1, rvi, &mut self.refc)?;
                    cur_frame.write_register(treg.r3, val, &mut self.refc)?;
                }
                op::PUTINDEX => {
                    let treg = op::TripleRegst::try_from(&mut slvw)?;
                    let rval =
                        cur_frame
                            .take_register(treg.r3, &mut self.refc)?
                            .ok_or(new_error(
                                ErrorType::EmptyRegister,
                                format!("Register {} is empty", treg.r3),
                            ))?;
                    let idxv =
                        cur_frame
                            .take_register(treg.r2, &mut self.refc)?
                            .ok_or(new_error(
                                ErrorType::EmptyRegister,
                                format!("Register {} is empty", treg.r2),
                            ))?;
                    let ctyp = cur_frame.read_mutregst(treg.r1)?;
                    cur_frame.regs[treg.r3 as usize] =
                        types::try_putindex(ctyp, &idxv, rval, &self.refc)?; // regs[r3] is None, so no need to write, or drop check.
                    cur_frame.regs[treg.r2 as usize] = Some(idxv); // Move the index back.
                }
                op::NEWLIST => {
                    let reg = slvw.get_u8();
                    let val = BaseType::Alloc(Box::new(types::CompositeType::List(Vec::new())));
                    cur_frame.write_register(reg, val, &mut self.refc)?;
                }
                op::NEWBITS => {
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    let rv1 = cur_frame.read_register(dreg.r1)?;
                    let size = match rv1 {
                        BaseType::Int(i) => {
                            if *i < 0 {
                                return Err(new_error(
                                    ErrorType::IllegalValue,
                                    format!("Value {i} is not a valid length."),
                                ));
                            } else {
                                *i as usize
                            }
                        }
                        i => {
                            return Err(new_error(
                                ErrorType::IllegalValue,
                                format!("Value {i} is not a valid length."),
                            ));
                        }
                    };
                    let val = BaseType::Alloc(Box::new(types::CompositeType::BitSet(
                        crate::utils::BitSet::new(size),
                    )));
                    cur_frame.write_register(dreg.r2, val, &mut self.refc)?;
                }
                op::NEWRANGE => {
                    let qreg = op::QuadrupleRegst::try_from(&mut slvw)?;
                    let r2 = cur_frame.read_register(qreg.r2)?;
                    let r3 = cur_frame.read_register(qreg.s1)?;
                    let r4 = cur_frame.read_register(qreg.s2)?;
                    let start = match r2 {
                        BaseType::Int(i) => *i,
                        _ => {
                            return Err(new_error(
                                ErrorType::TypeMismatch,
                                format!("Cannot create range with {r2} as start"),
                            ))
                        }
                    };
                    let end = match r3 {
                        BaseType::Int(i) => *i,
                        _ => {
                            return Err(new_error(
                                ErrorType::TypeMismatch,
                                format!("Cannot create range with {r3} as end"),
                            ))
                        }
                    };
                    let step = match r4 {
                        BaseType::Int(i) => *i,
                        _ => {
                            return Err(new_error(
                                ErrorType::TypeMismatch,
                                format!("Cannot create range with {r4} as end"),
                            ))
                        }
                    };
                    if step == 0 {
                        return Err(new_error(
                            ErrorType::IllegalValue,
                            "Cannot have zero step in range",
                        ));
                    }
                    let val =
                        BaseType::Alloc(Box::new(types::CompositeType::Range { start, end, step }));
                    cur_frame.write_register(qreg.r1, val, &mut self.refc)?;
                }
                op::NEWSTR => {
                    let reg = slvw.get_u8();
                    let val = BaseType::Alloc(Box::new(types::CompositeType::Str(String::new())));
                    cur_frame.write_register(reg, val, &mut self.refc)?;
                }
                op::ADVANCE => {
                    let dreg = op::DoubleRegst::try_from(&mut slvw)?;
                    let rv2 = cur_frame.read_register(dreg.r2)?;
                    let newadv = match rv2 {
                        BaseType::Int(i) => *i,
                        a => {
                            return Err(new_error(
                                ErrorType::IllegalValue,
                                format!("Value {a} is not a valid step."),
                            ));
                        }
                    };
                    let rv1 = cur_frame.read_mutregst(dreg.r1)?;
                    let ctype = types::as_composite_type_mut(rv1, &self.refc)?;
                    match ctype {
                        types::CompositeType::Slice {
                            parent: _,
                            start_off: _,
                            end_off: _,
                            advance_by,
                            reverse,
                        } => {
                            if newadv < 0 {
                                *reverse = !(*reverse);
                                *advance_by = (-newadv) as usize;
                            } else {
                                *advance_by = newadv as usize;
                            }
                        }
                        a => {
                            return Err(new_error(
                                ErrorType::TypeMismatch,
                                format!("Cannot set advance for {a}"),
                            ));
                        }
                    }
                }
                _ => {
                    return Err(new_error(
                        ErrorType::InvalidOpcode,
                        format!("Unrecognized opcode {opcode:x}"),
                    ));
                }
            }
            cur_frame.ip = slvw.offset();
        }
        Ok(())
    }
}
