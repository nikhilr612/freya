//! Module to traverse parsed S-expressions (AST) and generate VM modules.
//! Walkers in this module emit equivalent bytecode.

use std::{collections::HashMap, fs::File};
use crate::core::module::{ConstantValue, ExternDecl, FuncDecl};

use super::{parser::Sexpr, TlError, TlErrorType};

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


impl Scope<'_> {
	/// Create a new scope whose parent is this scope.
	fn child(&self) -> Scope<'_> {
		Scope {
			parent: Some(self),
			bindings: HashMap::new()
		}
	}

	/// Bind a symbol `name` to `stype` found at `offset`.
	/// Returns `Err` if name was already bound.
	fn bind(&mut self, name: &str, stype: SymbolType, offset: usize) -> Result<(), TlError> {
		if self.bindings.insert(name.to_owned(), stype).is_some() {
			Err(TlError { etype: TlErrorType::ReboundName, 
				msg: format!("Name `{name}` was already bound in this scope."),
				offset })
		} else {
			Ok(())
		}
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
	constants: HashMap<ConstantValue, usize>,
	xusenames: HashMap<String, String>,
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

pub fn walk(ast: Sexpr) -> Result<CompileUnit, TlError> {
	match ast {
		Sexpr::Atom(t) => {
			return Err(TlError { etype: TlErrorType::ExpectingList,
			 msg: "Root of AST must be a List S-expression.".to_owned(), 
			 offset: t.start });
		},
		Sexpr::List(v) => {
			for elm in v.iter() {
				todo!("Go through each element; verify that they are in-turn lists, and visit each list and check if it is define or use. Then, visit each function.");
			}
		}
	}	
	todo!("Implement root-level AST walker/visitor.")
}