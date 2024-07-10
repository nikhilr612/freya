//! Module to traverse parsed S-expressions (AST) and generate VM modules.
//! Walkers in this module emit equivalent bytecode.

use std::{collections::HashMap, fs::File};
use crate::core::module::{ExternDecl, FuncDecl};
use super::{Sexpr, SexprKind, TextualLocation, TlError, TlErrorType, Token};

#[derive(Clone)]
enum SymbolType {
	NamedValue { register: u8 },
	NamedConstant { pool_index: u16 },
	NamedInteger { value: i8 },
	InternalFunction { index: u16 },
	ExternalFunction { index: u16 }
}

#[derive(Default)]
/// Stores all name bindings.
struct Scope<'a> {
	/// Parent scope.
	parent: Option<&'a Scope<'a>>,
	/// Map symbols (or identifiers) to their corresponding type.
	bindings: HashMap<String, SymbolType>
}

fn not_ok<T>(opt: Option<T>) -> Result<(),T> {
	match opt {
		None => Ok(()),
		Some(a) => Err(a)
	}
}

impl Scope<'_> {
	/// Create a new scope whose parent is this scope.
	fn child(&self) -> Scope<'_> {
		Scope {
			parent: Some(self),
			bindings: HashMap::new()
		}
	}

	/// Bind a symbol `name` to `stype` found at `offset`.
	/// Returns `Err` if `name` was already bound.
	fn bind(&mut self, name: &str, stype: SymbolType) -> Result<(), SymbolType> {
		not_ok(self.bindings.insert(name.to_owned(), stype))
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

pub struct CompileUnit {
	functions: Vec<FuncDecl>,
	externfns: Vec<ExternDecl>,
	constants: HashMap<Token, usize>,
	xusenames: HashMap<String, (String, bool)>,
	tmpfile: File 
}

impl CompileUnit {
	fn new() -> Result<Self, std::io::Error> {
		Ok(Self {
			functions: Vec::new(),
			externfns: Vec::new(),
			constants: HashMap::new(),
			xusenames: HashMap::new(),
			tmpfile: tempfile::tempfile()?
		})
	}
}

fn visit_constant<'a>(mut it: impl Iterator<Item = &'a Sexpr>, loc: &TextualLocation) -> Result<Token, TlError> {
	let a = it.next()
		.ok_or_else(|| TlError {
			etype: TlErrorType::ExpectingAtom,
			msg: "Expecting literal atom to evaluate as constant value".to_owned(),
			loc: *loc
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

fn visit_define_form<'a>(mut it: impl Iterator<Item = &'a Sexpr>, cu: &mut CompileUnit, sc: &mut Scope<'_>, loc: &TextualLocation) -> Result<(), TlError> {
	let a = it
		.next()
		.ok_or_else(|| TlError { 
			etype: TlErrorType::InvalidForm,
			msg: "Incomplete `define` form, both head and body are missing.".to_owned(), 
			loc: *loc
		})?;
	match &a.kind {
		SexprKind::Atom(Token::Symbol(s)) => {
			let tok = visit_constant(it, loc)
				.map_err(|e| e.aug("Looking for literal atom as body for constant-type `define` form."))?;
			let mut id = cu.constants.len();
			if cu.constants.contains_key(&tok) {
				id = *cu.constants.get(&tok).unwrap();
			} else {
				cu.constants.insert(tok, id);
			}
			sc.bind(&s, SymbolType::NamedConstant { pool_index: id as u16 });
			todo!("Constant-type define form.")
		},
		SexprKind::List(_v) => {
			todo!("Function-type define form.")
		},
		_ => {
			return Err(TlError {
				etype: TlErrorType::IllegalAtom,
				msg: "Expecting either symbol or list as `define` form head.".to_owned(),
				loc: a.loc 
			});
		}
	}
}

fn visit_require_form<'a>(mut it: impl Iterator<Item = &'a Sexpr>, cu: &mut CompileUnit, loc: &TextualLocation) -> Result<(), TlError> {
	let path = it
		.next()
		.ok_or_else(|| TlError { 
			etype: TlErrorType::ExpectingAtom,
			msg: "Incomplete `require` form, expecting path string.".to_owned(), 
			loc: *loc
		})?
		.inspect_str()
		.map_err(|e| e.aug("Looking for `require` form path string."))?;
	let name = it
		.next()
		.ok_or_else(|| TlError {
			etype: TlErrorType::ExpectingAtom,
			msg: "Incomplete `require` form, expecting symbol name for binding".to_owned(),
			loc: *loc
		})?
		.inspect_symbol()
		.map_err(|e| e.aug("Looking for `require` form symbol name."))?;

	let is_native = match it.next() {
		None => false,
		Some(a) =>
			a.inspect_symbol().map_err(|e| e.aug("Looking for indicator symbol `:native` in require form."))? == ":native"
	};

	not_ok(cu.xusenames.insert(name.to_string(), (path.to_string(), is_native)))
		.map_err(|_| TlError {
			etype: TlErrorType::ReboundName,
			msg: format!("Require symbol `{name}` was already bound."),
			loc: *loc
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
		"require"=> visit_require_form(it, cu, &ast.loc),
		s => {
			Err(TlError { 
				etype: TlErrorType::IllegalAtom,
				msg: format!("Unexpected symbol {s}, root-level lists MUST be `define` or `require` forms."), 
				loc: a.loc 
			})
		}
	}
}

/// Consume the Abstract Syntax Tree represented by Sexpr `ast`, and return a `CompileUnit` after walking it.
pub fn walk(ast: Sexpr) -> Result<CompileUnit, TlError> {
	let mut global_scope = Scope::default();
	let mut compile_unit = CompileUnit::new().map_err(|e| TlError::from_ioerr(e, 0))?;
	let v = ast.list()?;

	// Visit each root-level list to get all defines, requires cleared up.
	// Doing this allows for mutually recursive functions to be defined without forward declarations and what not.
	TlError::collect(v.iter()
		.map(|t| visit_root_list(t, &mut compile_unit, &mut global_scope)), 
		"Failed to visit root-level lists.")?;

	// TODO: Walk each define.

	Ok(compile_unit)
}