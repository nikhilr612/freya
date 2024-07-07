//! Module to traverse parsed S-expressions (AST) and generate VM modules.
//! Walkers in this module emit equivalent bytecode.

use std::fs::File;
use super::parser::Sexpr;

pub fn walk(ast: Sexpr, tmpfile: File) {}