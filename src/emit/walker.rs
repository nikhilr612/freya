//! Module to traverse parsed S-expressions (AST) and generate VM modules.
//! Walkers in this module emit equivalent bytecode.
//!
//! ## Definitions
//! 1. __Atom__: Any of the tokens `Symbol`, `Integer`, `Float`, `Boolean`, `Char`, `Str` constitute an _atom_.
//! 2. __List__: A _list_ of _S-expressions_.
//! 3. __S-expression__: A value that is either a _List_ or an _Atom_ where the list is represented by a parenthesized string containing sub-S-expressions seperated by one or more whitespace characters.
//! 4. __Root-level S-expression__: An S-expression whose parent has no parent.
//! 5. __Literal Atom__: An atom that is not a `Symbol`.
//! 6. __Special Form__: A _List_ whose first element is one of the form-reserved symbols and whose other sub-S-expressions satisfy suitable corresponding constraints is termed _Special Form_ or simply _form_.
//! 7. __Indicator Symbol__: Any symbol conditionally matched by a special form.
//! 8. __External Symbol__: Any symbol that matches the string-pattern `"A:B"` where `A` is an external module declared in root-level `require` form, External symbols are resolved into `ExternDecl`
//! 9. __Regular List__: Any list which occurs as an S-expression (member or nested) in the body of a `func-type define` form.
//!10. __Regular Symbol__: Any symbol that occurs in a _regular list_ and is not the first element of that list.
//!11. __Regular S-expression__: Either a _regular symbol_ or a _regular list_

use super::{Sexpr, SexprKind, TextualLocation, TlError, TlErrorType, Token};
use crate::{
    core::{
        module::{ExternDecl, FuncDecl, MAGIC},
        op::{self, DoubleRegst, Id16Reg, QuadrupleRegst, VariadicRegst},
    },
    utils::{AsBytes, OutBuf},
};
use log::debug;
use std::path::Path;
use std::{
    collections::HashMap,
    fs::{File, OpenOptions},
    io::{Seek, Write},
};

#[derive(Debug, Clone)]
enum SymbolType {
    NamedValue { register: u8 },
    NamedConstant { pool_index: u16 },
    NamedInteger { value: i8 },
    InternalFunction { index: u16 },
}

#[derive(Default, Debug)]
/// Stores all name bindings.
struct Scope<'a> {
    /// Parent scope.
    parent: Option<&'a Scope<'a>>,
    /// Map symbols (or identifiers) to their corresponding type.
    bindings: HashMap<String, SymbolType>,
}

fn not_ok<T>(opt: Option<T>) -> Result<(), T> {
    match opt {
        None => Ok(()),
        Some(a) => Err(a),
    }
}

impl Scope<'_> {
    /// Create a new scope whose parent is this scope.
    fn child(&self) -> Scope<'_> {
        Scope {
            parent: Some(self),
            bindings: HashMap::new(),
        }
    }

    /// Bind a symbol `name` to `stype` found at `offset`.
    /// Returns `Err` if `name` was already bound.
    fn bind(&mut self, name: impl Into<String>, stype: SymbolType) -> Result<(), SymbolType> {
        not_ok(self.bindings.insert(name.into(), stype))
    }

    /// Search for `name` within current scope, and all parent scopes.
    /// Returns `None` if there is no binding `name`.
    fn resolve(&self, name: &str) -> Option<SymbolType> {
        let mut a = self;
        loop {
            let r = a.bindings.get(name);
            if r.is_some() {
                return r.cloned();
            }
            a = a.parent?;
        }
    }
}

#[derive(Debug)]
pub struct CompileUnit {
    functions: Vec<(String, FuncDecl)>,
    externfns: Vec<ExternDecl>,
    constants: HashMap<Token, usize>,
    xusenames: HashMap<String, (String, bool)>,
    externmap: HashMap<String, usize>,
    tmpfile: File,
}

impl CompileUnit {
    /// Create a new compile unit. Opens a temporary file.
    fn new() -> Result<Self, std::io::Error> {
        Ok(Self {
            functions: Vec::new(),
            externfns: Vec::new(),
            constants: HashMap::new(),
            xusenames: HashMap::new(),
            externmap: HashMap::new(),
            tmpfile: tempfile::tempfile()?,
        })
    }

    /// Write a simple instruction to the underlying file, comprising only of `opcode` and one octet operand (`v`)
    fn emit_simple(&mut self, opcode: u8, v: u8) -> Result<(), TlError> {
        self.tmpfile
            .write_u8(opcode)
            .map_err(CompileUnit::_write_error)?;
        self.tmpfile.write_u8(v).map_err(CompileUnit::_write_error)
    }

    fn emit_drop(&mut self, ralloc: &mut StackAllocator, r: u8) -> Result<(), TlError> {
    	self.emit_simple(op::DROP, r)?;
    	ralloc.rel(1);
    	Ok(())
    }

    /// Write an instruction with opcode `opcode` and operands specified by `content`.
    fn emit(&mut self, opcode: u8, content: impl AsBytes) -> Result<(), TlError> {
        self.tmpfile
            .write_u8(opcode)
            .map_err(CompileUnit::_write_error)?;
        content
            .write(&mut self.tmpfile)
            .map_err(CompileUnit::_write_error)
    }

    fn emit_lineno(&mut self, sx: &Sexpr) -> Result<(), TlError> {
        self.tmpfile
            .write_u8(op::DBGLN)
            .map_err(CompileUnit::_write_error)?;
        self.tmpfile
            .write_u16(sx.loc.line_no as u16)
            .map_err(CompileUnit::_write_error)
    }

    /// Get the stream position of the underlying file.
    fn offset(&mut self) -> u32 {
        self.tmpfile
            .stream_position()
            .expect("CompileUnit temporary file should have stream_position") as u32
    }

    fn _write_error(e: std::io::Error) -> TlError {
        TlError {
            etype: TlErrorType::IoError,
            msg: format!("Failed to write to temporary file backing compile unit,\n\tdetail: {e}"),
            loc: TextualLocation {
                start_offset: 0,
                end_offset: 0,
                line_no: 0,
            },
        }
    }

    /// Finish code-generation by creating the header, copying contents of instruction dump, and fixing all code-offsets.
    pub fn finish<F>(mut self, path: F) -> Result<(), std::io::Error>
    where
        F: AsRef<Path>,
    {
        let mut file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(true)
            .open(path)?;

        // TODO: Think of a better way of handling constants that doesn't need this, but at the same time doesn't make any copies of Token.
        // Token can potentially be incredibly large, since it can store file-size strings. This is important.
        let nconsts = self.constants.len();
        let mut v = vec![Token::Boolean(false); nconsts];
        for (token, idx) in self.constants {
            v[idx] = token;
        }

        file.write_all(&MAGIC)?;
        file.write_u16(nconsts as u16)?;

        for elm in v {
            match elm {
			    Token::Integer(v) => {
			    	file.write_u8(1)?;
			    	file.write_u64(v as u64)?;
			    },
			    Token::Float(v) => {
			    	file.write_u8(2)?;
			    	file.write_u64(v.to_bits())?;
			    },
			    Token::Char(v) => {
			    	file.write_u8(3)?;
			    	file.write_u32(v as u32)?;
			    },
			    Token::Str(v) => {
			    	file.write_u8(4)?;
			    	file.write_u16(v.len() as u16)?;
			    	file.write_str(&v)?;
			    },
			    Token::Symbol(v) => panic!("Somehow `{v}` made it into the constant pool. A symbol can't be a constant; it was checked for. Only literal atoms are constants."),
			    Token::Boolean(_)=> panic!("Boolean constants aren't even inserted into the pool in the first place. Somehow one made it in there.")
			}
        }

        file.write_u16(self.externfns.len() as u16)?;
        for elm in self.externfns {
            file.write_u8(elm.native as u8)?;
            let s = format!("{}:{}", elm.module_path, elm.func_name);
            file.write_u16(s.len() as u16)?;
            file.write_str(&s)?;
        }

        file.write_u16(self.functions.len() as u16)?;
        let header_end = (file.stream_position()? as u32)
            + (self.functions.iter().fold(0, |acc, x| acc + 7 + x.0.len()) as u32);
        for (fname, fdc) in self.functions {
            file.write_u8(fname.len() as u8)?;
            file.write_str(fname.as_str())?;
            file.write_u8(fdc.nparam)?;
            file.write_u8(fdc.nregs)?;
            file.write_u32(header_end + fdc.offset)?;
            debug!("Wrote function {fname} {fdc:?}")
        }

        let p = self.tmpfile.stream_position()?;
        self.tmpfile.rewind()?;
        if std::io::copy(&mut self.tmpfile, &mut file)? != p {
            eprintln!("Not all bytes written!");
        }

        Ok(())
    }
}

/// A struct to capture details related to evaluation of S-expr in a function definition.
/// Allocates virtual registers in LIFO manner, and tracks maximum number of registers needed.
#[derive(Debug, Default)]
struct StackAllocator {
    max: u8,
    cur: u8,
}

impl StackAllocator {
    fn many(&mut self, s: usize) -> std::ops::Range<u8> {
        let st = self.cur;
        self.cur += s as u8;
        if self.cur > self.max {
            self.max = self.cur
        }
        st..self.cur
    }

    fn get(&mut self) -> u8 {
        let r = self.cur;
        self.cur += 1;
        if self.cur > self.max {
            self.max = self.cur
        }
        r
    }

    fn rel(&mut self, s: u8) {
        if self.cur < s {
            panic!("Register allocation cockup; Trying to release more registers than available.");
        }
        self.cur -= s;
    }
}

fn visit_constant<'a>(
    mut it: impl Iterator<Item = &'a Sexpr>,
    loc: &TextualLocation,
) -> Result<Token, TlError> {
    let a = it.next().ok_or_else(|| TlError {
        etype: TlErrorType::ExpectingAtom,
        msg: "Expecting literal atom to evaluate as constant value".to_owned(),
        loc: *loc,
    })?;
    match &a.kind {
	    SexprKind::Atom(Token::Symbol(_)) => {
	    	Err(TlError {
	    		etype: TlErrorType::IllegalAtom,
	    		msg: "Symbols are not literal atoms that can be evaluated as a constant value".to_owned(),
	    		loc: a.loc
	    	})
	    },
	    SexprKind::Atom(t) => Ok(t.clone()),
	    SexprKind::List(_) => {
	    	Err(TlError { 
	    		etype: TlErrorType::ExpectingAtom,
	    		msg: "Expecting literal atom to evaluate as constant value. Found list. List literals are not atoms".to_owned(),
	    		loc: a.loc
	    	})
	    }
	}
}

/// The `define` form comprises of two sub-S-expressions - `head` and `body`.
/// * When `head` is a symbol, `define` form is said to be 'constant-type' or `const-type`.
/// * When `head` is a symbol list, `define` form is said to be 'function-type' or `func-type`
/// * Any other S-expressions constitute an invalid `define` form.
///
/// All `define` forms bind names into the global scope, and thus are _only matched when occurring as root-level lists_.
/// Function-type `define` forms are visited first to bind names into the global scope, after which their body is walked.
/// This eliminated the need for any kind of forward declaration, and functions are allowed to freely recurse with each other.
fn visit_define_form<'a>(
    mut it: impl Iterator<Item = &'a Sexpr>,
    cu: &mut CompileUnit,
    sc: &mut Scope<'_>,
    loc: &TextualLocation,
) -> Result<(), TlError> {
    let a = it.next().ok_or_else(|| TlError {
        etype: TlErrorType::InvalidForm,
        msg: "Incomplete `define` form, both head and body are missing.".to_owned(),
        loc: *loc,
    })?;
    match &a.kind {
        SexprKind::Atom(Token::Symbol(s)) => {
            let tok = visit_constant(it, loc).map_err(|e| {
                e.aug("Looking for literal atom as body for constant-type `define` form.")
            })?;
            let sym = match tok {
                Token::Integer(v) if (-0x80..=0x7f).contains(&v) => {
                    SymbolType::NamedInteger { value: v as i8 }
                }
                Token::Boolean(v) => SymbolType::NamedInteger { value: v as i8 },
                Token::Symbol(_) => {
                    panic!("Somehow the token which was already checked for is now a symbol.")
                }
                tok => {
                    let new_id = cu.constants.len();
                    let new_id = *cu.constants.entry(tok).or_insert(new_id);
                    SymbolType::NamedConstant {
                        pool_index: new_id as u16,
                    }
                }
            };
            sc.bind(s, sym).map_err(|_| TlError {
                etype: TlErrorType::ReboundName,
                msg: format!("Constant-type define form symbol `{s}` was already bound."),
                loc: a.loc,
            })
        }
        SexprKind::List(v) => {
            let name = v
                .first()
                .ok_or_else(|| TlError {
                    etype: TlErrorType::ExpectingAtom,
                    msg: "Expecting list of symbols for name and parameters, found empty list."
                        .to_owned(),
                    loc: a.loc,
                })?
                .inspect_symbol()
                .map_err(|e| {
                    e.aug("Looking for function name symbol in function-type `define` form.")
                })?;
            let nparam = (v.len() - 1) as u8;
            let fdc = FuncDecl {
                nparam,
                nregs: nparam,
                offset: 0,
            };
            let index = cu.functions.len() as u16;
            cu.functions.push((name.to_string(), fdc));
            sc.bind(name, SymbolType::InternalFunction { index })
                .map_err(|_| TlError {
                    etype: TlErrorType::ReboundName,
                    msg: format!("Function-type define form symbol `{name}` was already bound."),
                    loc: a.loc,
                })
        }
        _ => Err(TlError {
            etype: TlErrorType::IllegalAtom,
            msg: "Expecting either symbol or list as `define` form head.".to_owned(),
            loc: a.loc,
        }),
    }
}

/// The require form binds a path-string to an external module to a name in the current unit.
/// The require form has two 2-3 sub-S-expressions - `(require path name indicator)`.
/// * `path` must always be a `String` atom.
/// * `name` must always be a `Symbol` atom.
/// * `indicator` is an optionally matched sub-S-expression which when found MUST be a symbol that is one of: `:native`, `:module`, or `:rawstr`.
/// If indicator is not specified, the result is the same as if the indicator `:module` was specified.
///
/// If the indicator is `:native`, all external functions from this name will be presmued as native `ExternDecl`, i.e, native functions.
///
/// If the indicator is `:module`, all external functions from this name will be non-native, VM functions.
///
/// If the indicator is `:rawstr`, the contents of file specified by `path` is read into a `Str` which is then bound to `name`. This variant is equivalent to a `const-type define` form with the contents of the file as a `Str` literal atom 'body'.
fn visit_require_form<'a>(
    mut it: impl Iterator<Item = &'a Sexpr>,
    cu: &mut CompileUnit,
    loc: &TextualLocation,
) -> Result<(), TlError> {
    let path = it
        .next()
        .ok_or_else(|| TlError {
            etype: TlErrorType::ExpectingAtom,
            msg: "Incomplete `require` form, expecting path string.".to_owned(),
            loc: *loc,
        })?
        .inspect_str()
        .map_err(|e| e.aug("Looking for `require` form path string."))?;
    let name = it
        .next()
        .ok_or_else(|| TlError {
            etype: TlErrorType::ExpectingAtom,
            msg: "Incomplete `require` form, expecting symbol name for binding".to_owned(),
            loc: *loc,
        })?
        .inspect_symbol()
        .map_err(|e| e.aug("Looking for `require` form symbol name."))?;

    let is_native = match it.next() {
        None => false,
        Some(a) => {
            let s = a
                .inspect_symbol()
                .map_err(|e| e.aug("Looking for indicator symbol in require form."))?;
            match s {
				":rawstr" => todo!("Implement including files as raw strings in constant pool."),
				":native" => true,
				":module" => false,
				_ => {
					return Err(TlError {
				        etype: TlErrorType::IllegalAtom,
				        msg: "The only valid indicator symbols in `require` form are `:native`, `:rawstr`, and `:module`".to_owned(),
				        loc: a.loc,
				    })
				}
			}
        }
    };

    not_ok(
        cu.xusenames
            .insert(name.to_string(), (path.to_string(), is_native)),
    )
    .map_err(|_| TlError {
        etype: TlErrorType::ReboundName,
        msg: format!("Require symbol `{name}` was already bound."),
        loc: *loc,
    })
}

fn visit_root_list(ast: &Sexpr, cu: &mut CompileUnit, sc: &mut Scope<'_>) -> Result<(), TlError> {
    let mut it = ast.iter()?;
    let a = it
		.next()
		.ok_or_else(|| TlError { 
			etype: TlErrorType::EmptyList,
			msg: "Root-level lists MUST be either `define` or `require` forms. Empty lists are not allowed here.".to_owned(), 
			loc: ast.loc 
		})?;
    let symbol = a
        .inspect_symbol()
        .map_err(|e| e.aug("Looking for root-level `define` or `require` forms."))?;
    match symbol {
        "define" => visit_define_form(it, cu, sc, &ast.loc),
        "require" => visit_require_form(it, cu, &ast.loc),
        s => Err(TlError {
            etype: TlErrorType::IllegalAtom,
            msg: format!(
                "Unexpected symbol {s}, root-level lists MUST be `define` or `require` forms."
            ),
            loc: a.loc,
        }),
    }
}

/// Consuming a literal atom involves:
/// 1. attempting to obtain its index in the constant pool.
/// 2. or on failing, inserting it into the constant pool.
///
/// Then emit `LDC` to store value into register `r`.
fn eat_literal_atom(t: Token, cu: &mut CompileUnit, r1: u8) -> Result<(), TlError> {
    match t {
        Token::Integer(i) if i > -0x80 && i < 0x7f => {
            return cu.emit(
                op::LDI,
                DoubleRegst {
                    r1: (i as i8) as u8,
                    r2: r1,
                },
            );
        }
        Token::Boolean(i) => {
            return cu.emit(
                op::LDI,
                DoubleRegst {
                    r1: (i as i8) as u8,
                    r2: r1,
                },
            );
        }
        _ => {}
    }
    let id = cu.constants.len();
    let id = *cu.constants.entry(t).or_insert(id) as u16;
    cu.emit(op::LDC, Id16Reg { id, r1 })
}

/// Walk a func-type `define` form. Does nothing if `define` form is const-type.
/// Verifies that `(name param..)` list (i.e, the `head`) comprises only of symbols and then walks the `body`.
///
/// There are essentially two types of func-type `define` forms` :
/// 1. _Stub_ : when the body is an atom.
/// 2. _Extended_ : when the body is a list.
///
/// In case of _Stub_ type, _all_ formal parameters need **not** be bound.
/// Even if the atom refers to one, only the first instance of that formal parameter need be bound.
/// This allows for functions like
/// ```scheme
/// (define (first x _ _ _) x)
/// (define (seq1 _) 1)
/// ; and also
/// (define (second x y y x) y) ; this always returns the second parameter.
///
/// ```
///
/// In case of _Extended_ type, **all** formal parameters are bound.
/// This allows for functions like
/// ```scheme
/// (define (mad x y z) (+ z (* x y)))
/// ; and seems to forbid
/// (define (print-first x _ _) (print x)) ; as `_` would be bound twice, but is instead ignored.
/// ```
/// # Impl Considerations
/// There are two things apparent from the preceding section:
/// 1. Although _Stub_ type forms allow for 'elegant' definition of simple functions, under the current rules, it introduces syntax that can easily be misused.
/// To prevent this, it may be worthwhile, to first verify contents of the parameter list seperately, ensuring that they are all symbols, for one.
/// 2. The syntax for _Extended_ type is not amenable for misuse; however, it may lend to needless verbosity in simple cases.
/// One way to fix this issue would be to temporarily grant special privilege to the symbol `_` to imply an absence of binding.
fn walk_define_form(
    v: Vec<Sexpr>,
    cu: &mut CompileUnit,
    sc: &mut Scope<'_>,
) -> Result<(), TlError> {
    let mut v = v.into_iter().skip(1);
    let b = v
        .next()
        .expect("Validated `define` form should have head Sexpr");
    let c = v
        .next()
        // This one might not have been checked in visit, since func-type defines on visit are only concerned with name and number of args.
        .ok_or_else(|| TlError {
            etype: TlErrorType::InvalidForm,
            msg: "Expecting `define` form body.".to_owned(),
            loc: b.loc,
        })?;

    // Try to get a list out.
    let b = b.list();
    // Again, constant-type define was already handled previously
    if b.is_err() {
        return Ok(());
    }
    let mut b = b.unwrap().into_iter().map(|f| f.symbol());

    let name = b
        .next()
        .expect("Validated func-type `define` should have atleast 1 member in head")
        .expect("Validated func-type `define` head's 1st memeber should have been a symbol");
    // |--> the last 'expect' for now, since everything here after was untouched while visiting.

    let fparams = b.collect::<Result<Vec<_>, _>>().map_err(|e| {
        e.aug("Looking for (func-name param..) symbol list as function-type `define` head.")
    })?;

    let fidx = if let SymbolType::InternalFunction { index } = sc
        .resolve(&name)
        .expect("Function name should already be bound.")
    {
        cu.functions[index as usize].1.offset = cu.offset();
        index as usize
    } else {
        panic!("Somehow function name {name} was bound as something that's not a function");
    };

    match c.kind {
        // Special rules for stub functions need to be dealt with here.
        SexprKind::Atom(Token::Symbol(s)) => {
            if s == "None" {
                cu.tmpfile
                    .write_u8(op::VRET)
                    .map_err(CompileUnit::_write_error)
            } else if let Some(SymbolType::NamedConstant { pool_index }) = sc.resolve(&s) {
                let fdc = &mut cu.functions[fidx].1;
                fdc.nregs = fdc.nregs.max(1);

                cu.emit(
                    op::LDC,
                    Id16Reg {
                        id: pool_index,
                        r1: 0,
                    },
                )?;
                cu.emit_simple(op::RET, 0)
            } else {
                let mut it = fparams
                    .iter()
                    .enumerate()
                    .filter(|&(_, p)| p == &s)
                    .map(|(a, _)| a)
                    .take(2);
                let r = it.next().ok_or_else(|| TlError {
                    etype: TlErrorType::UnknownSymbol,
                    msg: format!("`{s}` was not found in scope"),
                    loc: c.loc,
                })? as u8;
                not_ok(it.next()).map_err(|_| TlError {
                    etype: TlErrorType::ReboundName,
                    msg: format!("`{s}` was already bound."),
                    loc: c.loc,
                })?;
                cu.emit_simple(op::RET, r)
            }
            // FuncDecl has enough registers.
        }
        // Literal atom.
        SexprKind::Atom(t) => {
            let fdc = &mut cu.functions[fidx].1;
            fdc.nregs = fdc.nregs.max(1);
            eat_literal_atom(t, cu, 0)?;
            cu.emit_simple(op::RET, 0)
        }
        SexprKind::List(v) => {
            let mut ralloc = StackAllocator::default();
            let mut function_scope = sc.child();
            for (register, name) in ralloc.many(fparams.len()).zip(fparams.into_iter()) {
                if name == "_" {
                    continue;
                }
                function_scope
                    .bind(name, SymbolType::NamedValue { register })
                    .map_err(|_| TlError {
                        etype: TlErrorType::ReboundName,
                        msg:
                            "Duplicate binding names are not allowed for function formal parameters"
                                .to_owned(),
                        loc: c.loc,
                    })?;
            }
            let r = walk_regular_list(v, &mut ralloc, cu, &mut function_scope, c.loc, false)?;
            cu.functions[fidx].1.nregs = ralloc.max;
            match r {
                None => cu
                    .tmpfile
                    .write_u8(op::VRET)
                    .map_err(CompileUnit::_write_error),
                Some((r, _)) => cu.emit_simple(op::RET, r),
            }
        }
    }
}

fn check_regs_adjacent(v: &[u8]) -> bool {
    for (a, b) in v.iter().zip(v.iter().skip(1)) {
        if (b - a) != 1 {
            return false;
        }
    }
    true
}

/// Walk `print` form. Takes exactly one sub-S-expression - the value to print.
/// In effect `print` is a _procedure_, not a pure function.
fn walk_print_form(
    it: impl Iterator<Item = Sexpr>,
    ralloc: &mut StackAllocator,
    cu: &mut CompileUnit,
    sc: &mut Scope<'_>,
    loc: TextualLocation,
) -> Result<(), TlError> {
    let mut it = it.take(2);
    let ast = it.next().ok_or_else(|| TlError {
        etype: TlErrorType::InvalidForm,
        msg: "Incomplete print form; value not specified.".to_owned(),
        loc,
    })?;
    not_ok(it.next()).map_err(|_| TlError {
        etype: TlErrorType::InvalidForm,
        msg: "Print form takes exactly one sub-S-expression; found another.".to_owned(),
        loc,
    })?;
    let (r, t) = walk_regular_sexpr(ast, ralloc, cu, sc)?;
    cu.emit_simple(op::PRINT, r)?;
    if t {
    	cu.emit_drop(ralloc, r)?;
    }
    Ok(())
}

/// Similar to [walk_regular_sexpr] except that it doesn't require sexpr to yield values.
fn walk_opt_sexpr(
	ast: Sexpr,
    ralloc: &mut StackAllocator,
    cu: &mut CompileUnit,
    sc: &mut Scope<'_>
) -> Result<Option<(u8, bool)>, TlError> {
	match ast.kind {
        SexprKind::Atom(Token::Symbol(s)) => {
        	Ok(Some(walk_regular_symbol(s, ralloc, cu, sc, ast.loc)?))
        },
        SexprKind::Atom(t) => {
            let r = ralloc.get();
            eat_literal_atom(t, cu, r)?;
            // TODO: Consider adding a way for literal atoms to not be cleaned up.. Although this may be pointless.
            Ok(Some((r, true)))
        }
        SexprKind::List(v) => {
            let r = walk_regular_list(v, ralloc, cu, sc, ast.loc, true)?;
            Ok(r)
        }
    }
}

fn walk_begin_form(
	mut it: impl Iterator<Item = Sexpr>,
    ralloc: &mut StackAllocator,
    cu: &mut CompileUnit,
    sc: &mut Scope<'_>,
    loc: TextualLocation
) -> Result<(u8, bool), TlError> {
	let v = it.next().ok_or_else(|| TlError {
		etype: TlErrorType::InvalidForm,
		msg: "Incomplete begin form; requires at least one S-expression".to_owned(),
		loc
	})?;
	let mut last_eval = walk_opt_sexpr(v, ralloc, cu, sc)?;
	for v in it {
		if let Some((u, true)) = last_eval {
			cu.emit_drop(ralloc, u)?
		}
		last_eval = walk_opt_sexpr(v, ralloc, cu, sc)?;
	}
	last_eval.ok_or_else(|| TlError {
		etype: TlErrorType::InvalidForm,
		msg: "Last S-expression in `begin` form must be regular".to_owned(),
		loc
	})
}

/// Walk a 'regular' list `(s0 s1 s2 .. sN)` occurring as an S-expr in (i.e, member of, or nested in) a function body.
/// Returns the register containing the return value.
/// If the regular list is a special form that does not evaluate to a value, then return `Ok(None)`
///
/// If `s0` is itself a list - then this regular list performs a function call by:
/// ```other
/// walk((s0 s1 s2 .. sN), M):
///     rM <- walk(s0, M)
///     r(M+1) <- walk(s1, M+1)
///     r(M+2) <- walk(s2, M+2)
///     ..
///     r(M+N) <- walk(sN, M+N)
///     rM <- rM(r(M+1), r(M+2), ..., r(M+N))
/// ```
/// Clearly, if any of the sub-expressions `s1,..,sN` are symbols which refer to previously bound values, storing in sequential registers will not be possible.
/// This should be accounted for. Furthermore, any sub-expressions that are lists need to be evaluated with 'temporary' values.
///
/// If `s0` is a symbol then:
/// 1. Walk corresponding form if it matches any of the hard-reserved symbols (eg. `let`, `do`, `+`, `-`, `&imt`, `&mut`, etc.)
/// 2. If the symbol resolves to a register binding, or a function in global scope - the list performs a function call.
///
/// If `s0` is a literal atom, then return `Err`.
/// # Impl Consideration
/// Since the VM supports mutable, and immutable references at some point expression such as the ones below will be encountered.
/// ```scheme
/// (define (fun a) (bar (foo (&imt a)) (&mut a)))
/// ```
/// In particular, immutable references in nested calls must be released. Mutable references do not merit this treatment since they have move semantics.
/// Immutable references that have not been bound should be dropped.
/// The example would ideally require this (pseudo):
/// ```other
/// r0 : `a`
/// r1 <- `bar`
/// r2 <- `foo`
/// r3 <- &imt r0
/// r2 <- r2(r3)
/// drop r3
/// r3 <- &mut r0
/// r1 <- r1(r2, r3)
/// ret r1
/// ```
/// Ofcourse, an easy way to resolve this would be to track the temporary values and drop them immediately. This results in:
/// ```other
/// r0 : `a`
/// r1 <- `bar`
/// r2 <- `foo`
/// r3 <- &imt r0
/// r2 <- r2(r3)
/// drop r3
/// r3 <- &mut r0
/// r1 <- r1(r2, r3)
/// drop r2
/// drop r3
/// ret r1
/// ```
/// The last drops can be skiped, realizing that S-expr is not nested; leading to the original.
/// Since value types cannot be inspected at compile time, often this strategy will result in redundant drops.
/// Furthermore, should these temporary values be stored in contiguous registers, a special instruction may be used.
///
/// ```other
/// ; drops registers A to B in sequence; A <= B
/// del rA, rB
/// ```
fn walk_regular_list(
    slist: Vec<Sexpr>,
    ralloc: &mut StackAllocator,
    cu: &mut CompileUnit,
    sc: &mut Scope<'_>,
    loc: TextualLocation,
    cleanup: bool,
) -> Result<Option<(u8, bool)>, TlError> {
    let mut it = slist.into_iter();

    let s0 = it.next().ok_or_else(|| TlError {
        etype: TlErrorType::EmptyList,
        msg: "Found Empty list when attempting to walk regular list.".to_owned(),
        loc,
    })?;

    cu.emit_lineno(&s0)
        .map_err(|e| e.aug("Trying to emit line number debug information"))?;

    let (callable_r, is_temp) = match s0.kind {
	    SexprKind::Atom(Token::Symbol(s)) => {
	    	match s.as_str() {
	    		// Reserved forms
	    		"print" => {
	    			walk_print_form(it, ralloc, cu, sc, loc)?;
	    			return Ok(None);
	    		},
	    		"begin" => {
	    			let r = walk_begin_form(it, ralloc, cu, sc, loc)?;
	    			return Ok(Some(r));
	    		},
	    		_ => walk_regular_symbol(s, ralloc, cu, sc, s0.loc)
	    	}
	    },
	    SexprKind::Atom(t) => {
	    	return Err(TlError { 
	    		etype: TlErrorType::IllegalAtom,
	    		msg: format!("{t} is not a symbol. The first sub-expression in a regular list must either be a symbol, or a list (which would be regular)."),
	    		loc: s0.loc 
	    	})
	    }
	    SexprKind::List(v) => {
	    	// This nested procedure call will cleanup it's own mess, so we don't have to,
	    	walk_regular_list(v, ralloc, cu, sc, s0.loc, true)
	    		.map_err(|e| e.aug("Trying to walkk nested regular list."))?
	    		.ok_or_else(|| TlError {
			        etype: TlErrorType::IllegalList,
			        msg: "Nested regular list must return value".to_owned(),
			        loc: s0.loc,
			    })
	    },
	}?;

    let eval_reg = if is_temp { callable_r } else { ralloc.get() };
    let mut nt = 0;
    let v = TlError::map_collect(
        it.map(|p| walk_regular_sexpr(p, ralloc, cu, sc)),
        |(r, t)| {
            nt += t as usize;
            r
        },
        "Failed to walk arguments for function call specified by regular list.",
    )?;
    if v.is_empty() {
        // No arg function call.
        cu.emit_simple(op::STDCALL, callable_r)?;
        cu.emit_simple(eval_reg + 1, 0)?;
    } else if check_regs_adjacent(&v) {
        let s1 = v.first().copied().unwrap();
        let s2 = v.last().copied().unwrap();
        cu.emit(
            op::FSTCALL,
            QuadrupleRegst {
                r1: callable_r,
                r2: (eval_reg + 1),
                s1,
                s2,
            },
        )?;
    } else {
        cu.emit(
            op::STDCALL,
            DoubleRegst {
                r1: callable_r,
                r2: (eval_reg + 1),
            },
        )?;
        let v: VariadicRegst = v.into();
        v.write(&mut cu.tmpfile)
            .map_err(CompileUnit::_write_error)?;
    }

    // Release all temporary registers.
    let nt = nt as u8;
    ralloc.rel(nt);

    if cleanup {
        // In case of stack - these registers will be adjacent.
        cu.emit(
            op::DEL,
            DoubleRegst {
                r1: eval_reg + 1,
                r2: eval_reg + nt,
            },
        )?;
    }

    // Function calls will always give out temporary registers.
    // The same cannot be said for other forms like `define` for instance.
    Ok(Some((eval_reg, true)))
}

/// Check if this symbol is an external one.
/// Loads it into a register if it was resolved successfully.
/// Returns `Err` if any error occurred while writing.
fn walk_extern_symbol(
    s: &str,
    ralloc: &mut StackAllocator,
    cu: &mut CompileUnit,
) -> Result<Option<u8>, TlError> {
    let a = cu.externmap.get(s).copied().or_else(|| {
        let (a, b) = s.rsplit_once(':')?;
        let (mpath, native) = cu.xusenames.get(a)?;
        let id = cu.externfns.len();
        cu.externfns.push(ExternDecl::new_extern(mpath, b, *native));
        cu.externmap.insert(s.to_string(), id);
        Some(id)
    });
    let id = match a {
        Some(i) => i as u16,
        None => return Ok(None),
    };
    let r1 = ralloc.get();
    cu.emit(op::LDX, Id16Reg { id, r1 })?;
    Ok(Some(r1))
}

/// A 'regular' symbol is one which is either an external symbol, `None`, or a name in current scope.
/// Importantly, special forms are _not regular_, and are contextually matched. Effectively, all 'reserved symbols' used in special forms are _soft keywords_.
/// To illustrate this, note that the following is perfectly valid:
/// ```scheme
/// (define print "Yo!")
/// ; The first print is matched as the reserved symbol for print form, while the second is a regular symbol.
/// (define (main) (print print))
/// ```
/// Returns true if register was allocated, false if symbol refers to a named value previously allocated.
fn walk_regular_symbol(
    s: String,
    ralloc: &mut StackAllocator,
    cu: &mut CompileUnit,
    sc: &mut Scope<'_>,
    loc: TextualLocation,
) -> Result<(u8, bool), TlError> {
    if s == "None" {
        // Technically stored in a temporary variable.
        let r = ralloc.get();
        Ok((r, true))
    } else {
        if let Some(r) = walk_extern_symbol(&s, ralloc, cu)? {
            return Ok((r, true));
        }
        match sc.resolve(&s) {
            Some(SymbolType::NamedValue { register }) => Ok((register, false)),
            Some(SymbolType::NamedConstant { pool_index }) => {
                let r1 = ralloc.get();
                cu.emit(op::LDC, Id16Reg { id: pool_index, r1 })?;
                Ok((r1, true))
            }
            Some(SymbolType::NamedInteger { value }) => {
                let r2 = ralloc.get();
                cu.emit(
                    op::LDI,
                    DoubleRegst {
                        r1: value as u8,
                        r2,
                    },
                )?;
                Ok((r2, true))
            }
            Some(SymbolType::InternalFunction { index }) => {
                let r1 = ralloc.get();
                cu.emit(op::LDF, Id16Reg { id: index, r1 })?;
                Ok((r1, true))
            }
            None => {
            	debug!("Current scope: {sc:#?}");
            	Err(TlError {
                	etype: TlErrorType::UnknownSymbol,
                	msg: format!("Unknown symbol `{s}` as atom in regular sexpr"),
                	loc,
            	})
            },
        }
    }
}

/// Walk a regular S-expr `ast`.
/// Returns the register containing the eval value of `ast` and a boolean flag indicating whether the register is 'temporary'.
/// All registers are temporary, unless they contain bound values (in otherwords registers bound to names).
fn walk_regular_sexpr(
    ast: Sexpr,
    ralloc: &mut StackAllocator,
    cu: &mut CompileUnit,
    sc: &mut Scope<'_>,
) -> Result<(u8, bool), TlError> {
    match ast.kind {
        SexprKind::Atom(Token::Symbol(s)) => walk_regular_symbol(s, ralloc, cu, sc, ast.loc),
        SexprKind::Atom(t) => {
            let r = ralloc.get();
            eat_literal_atom(t, cu, r)?;
            // TODO: Consider adding a way for literal atoms to not be cleaned up.. Although this may be pointless.
            Ok((r, true))
        }
        SexprKind::List(v) => {
            walk_regular_list(v, ralloc, cu, sc, ast.loc, true)?
            	.ok_or_else(|| TlError {
	                etype: TlErrorType::IllegalList,
	                msg: "A regular list that occurs as a regular expression must evaluate to a value".to_owned(),
	                loc: ast.loc,
	            })
        }
    }
}

fn walk_root_list(ast: Sexpr, cu: &mut CompileUnit, sc: &mut Scope<'_>) -> Result<(), TlError> {
    // Using `expect` here instead `?` since these errors should really not happen here...
    let v = ast
        .list()
        .expect("Visit pass should have error-ed out on illegal root-level lists.");
    let s = v
        .first()
        .expect("Root-level list, after visiting should have atleast 1 element (symbol)")
        .inspect_symbol()
        .expect("Visit pass should have error-ed out on illegal root-level lists.");
    if s != "define" {
        return Ok(());
    } // Other forms (`require`) should be handled in visit phase itself.
    walk_define_form(v, cu, sc)
}

/// Consume the Abstract Syntax Tree represented by Sexpr `ast`, and return a `CompileUnit` after walking it.
pub fn walk(ast: Sexpr) -> Result<CompileUnit, TlError> {
    let mut global_scope = Scope::default();
    let mut compile_unit = CompileUnit::new().map_err(|e| TlError::from_ioerr(e, 0))?;
    let v = ast.list()?;

    // Visit each root-level list to get all `define`s, `require`s cleared up.
    // Doing this allows for mutually recursive functions to be defined without forward declarations and what not.
    TlError::capture(
        v.iter()
            .map(|t| visit_root_list(t, &mut compile_unit, &mut global_scope)),
        "Failed to visit root-level lists.",
    )?;

    // Consume root-level Sexpr, and walk function-type defines.
    TlError::capture(
        v.into_iter()
            .map(|t| walk_root_list(t, &mut compile_unit, &mut global_scope)),
        "Failed to walk function-type `define` forms that are root-level lists.",
    )?;

    // TODO: Walk each define.
    debug!("Gloal scope: {global_scope:#?}");
    Ok(compile_unit)
}
