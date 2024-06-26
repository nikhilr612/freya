//! Module for SExpr parser. This parser does not handle unicode characters. ASCII only.
//! Builds a tree of tokens.

// TODO: Extremely verbose. Desperately needs a re-write.

use std::{io::Read, path::Display};

/// Enum for all possible token types.
#[derive(Debug)]
enum TokenType {
	/// Integer literal.
	Integer(i64),
	/// Boolean literal.
	Boolean(bool),
	/// Float/Decimal literal.
	Float(f64),
	/// Character literal.
	Char(u8),
	/// String literal.
	Str(String),
	/// Symbol token.
	Symbol(String)
}

/// Actual token read from input.
/// Contains line number, and start character.
#[derive(Debug)]
pub struct Token {
	/// The token type.
	ttype: TokenType,
	/// Start offset.
	start: usize,
	/// End offset.
	end: usize
}

#[derive(Debug)]
/// Structure for parsing S-expressions.
pub enum Sexpr {
	Atom(Token),
	List(Vec<Sexpr>)
}

/*
One way to do this would be to define a finite-state machine for the express purpose of tokenizing the stream.
Then, define a parser function to recursively (or using a stack backbone) group tokens into Sexpr and build AST.
enum ParserState {
	/// The parser awaits a token, or LPARAM ('(')
	Open,
	/// The parser has recorded only digits thus far. If token terminates, then an integer is emitted.
	Number,
	/// The parser has recorded a `.` somewhere, and a float will be emitted.
	FloatN,
	/// The parser has recorded alphanumeric characters, that signal a valid identifier thus far. Emits a symbol.
	Symbol,
	/// The parser is in the process of consuming a string literal. Escape sequences will be translated here.
	Quotes
}
But, this approach is not taken here.
*/

/// An aggregate enum with error codes for all errors involved in parsing, and code emitting phases.
#[repr(u8)]
#[derive(Debug)]
pub enum TlErrorType {
	IoError,
	//UnexpectedToken,
	UnexpectedEOF,
	InvalidNumeric,
	InvalidBoolean
}

pub struct TlError {
	etype: TlErrorType,
	offset: usize
}

impl std::fmt::Display for TlError {
	fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
		write!(fmt, "TlError [{:?}] @ {}", self.etype, self.offset)
	}
}

const NUMERIC_LITERAL_MAX_CHARS: usize = 48;
const SYMBOL_MAXW_CHARS: usize = 56;
const STRING_LITERAL_MAXW_CHARS: usize = 2048;

macro_rules! tl_error {
	($etype: ident, $offset: expr) => {
		Err(TlError { etype: TlErrorType::$etype, offset: $offset })
	};
}

fn _parse_number(lead: u8, reader: &mut impl Read, offset: usize) -> Result<Token, TlError> {
	// Ugly, but works I suppose. Ugly nonetheless..
	// TODO: Fix this, if possible.

	let mut charbuffer = [0u8; NUMERIC_LITERAL_MAX_CHARS];
	let mut p = 1;	// Definitely not very Rust-like. 
	
	let mut is_float = false;

	charbuffer[0] = lead;
	for byte in reader.bytes() {
		if p >= NUMERIC_LITERAL_MAX_CHARS {
			return tl_error!(InvalidNumeric, offset + p);
		}
		match byte {
			Ok(c) => {
				if c.is_ascii_digit() {
					charbuffer[p] = c;
				} else if c == b'.' || c == b'E' || c == b'e' {
					is_float = true;
					charbuffer[p] = c;
				} else if c.is_ascii_whitespace() {
					break;
				} else {
					return tl_error!(InvalidNumeric, offset + p);
				}
				p += 1;
			},
			Err(e) => {
				eprintln!("IoError while reading byte. {e:?}");
				return tl_error!(IoError, offset + p);
			}
		}
	}

	let s = core::str::from_utf8(&charbuffer[..p]).expect("Numeric type parser does not have valid charbuffer.");
	p += offset;

	if is_float {
		let v: f64 = s.parse().or_else(|_| tl_error!(InvalidNumeric, p))?;
		Ok(Token { ttype: TokenType::Float(v), start: offset, end: p })
	} else {
		let v: i64 = s.parse().or_else(|_| tl_error!(InvalidNumeric, p))?;
		Ok(Token { ttype: TokenType::Integer(v), start: offset, end: p })
	}
}

fn _parse_boolean(reader: &mut impl Read, offset: usize) -> Result<Token, TlErrorType> {
	let nch = reader.bytes().next().ok_or(TlErrorType::UnexpectedEOF)?.map_err(|e| {
		eprintln!("IoError while reading byte. {e:?}");
		TlErrorType::IoError
	})?;
	match nch {
		b't' => Ok(Token { ttype: TokenType::Boolean(true), start: offset, end: offset + 1 }),
		b'f' => Ok(Token { ttype: TokenType::Boolean(false), start: offset, end: offset + 1 }),
		_ => Err(TlErrorType::InvalidBoolean)
	}
}

fn _parse_symbol(lead: u8, reader: &mut impl Read, offset: usize) -> Result<Token, TlError> {

	let mut p = offset + 1;
	// Initialize char buffer.
	let mut s = String::with_capacity(SYMBOL_MAXW_CHARS >> 1);
	s.push(lead as char);
	
	for byte in reader.bytes() {
		match byte {
			Err(e) => {
				eprintln!("IoError while reading byte. {e:?}");
				return tl_error!(IoError, p);
			},
			Ok(u) => {
				if u.is_ascii_whitespace() { break; }
				s.push(u as char);
				p += 1;
				if s.len() > SYMBOL_MAXW_CHARS {
					eprintln!("WARNING: Symbol \"{}..\" is too long. Consider renaming.", &s[..10]);
				}
			}
		}
	}
	Ok(Token { ttype: TokenType::Symbol(s), start: offset, end: p})
}

fn _parse_escape_seq(reader: &mut impl Read, offset: usize) -> Result<char, TlError>{
	match reader.bytes().next() {
		Some(Ok(u)) => {
			match u {
				b't' => Ok('\t'),
				b'n' => Ok('\n'),
				b'r' => Ok('\r'),
				a => Ok(a as char)
			}
		},
		Some(Err(e)) => {
			eprintln!("IoError while reading byte for escape sequence. {e:?}");
			tl_error!(IoError, offset)
		},
		None => {
			tl_error!(UnexpectedEOF, offset)
		}
	}
}

#[inline]
fn _read_single_u8(reader: &mut impl Read, offset: usize) -> Result<u8, TlError> {
	let mut single = [0u8];
	match reader.read(&mut single) {
		Ok(1) => Ok(single[0]),
		Err(e) => {
			eprintln!("IoError while reading byte. {e:?}");
			tl_error!(IoError, offset)
		},
		_ => tl_error!(UnexpectedEOF, offset)
	}
}

fn _parse_string(reader: &mut impl Read, offset: usize) -> Result<Token, TlError> {
	
	let mut p = offset;
	let mut s = String::with_capacity(STRING_LITERAL_MAXW_CHARS >> 2);

	loop {
		let byte = reader.bytes().next();
		match byte {
			Some(Ok(a)) => {
				if a == b'"' {
					break;
				} else if a == b'\\' {
					s.push(_parse_escape_seq(reader, p)?);
					p += 1;
				} else {
					s.push(a as char);
				}
				p += 1;
			},
			Some(Err(e)) => {
				eprintln!("IoError while reading byte. {e:?}");
				return tl_error!(IoError, p);
			},
			None => {
				return tl_error!(UnexpectedEOF, p);
			}
		}
	}

	Ok(Token { ttype: TokenType::Str(s), start: offset, end: p })
}

// TODO: Fix all token parsing rules to terminate properly. Parentheses are included in token, this is wrong.
fn _match_rule(byte: u8, reader: &mut impl Read, offset: &mut usize) -> Result<Sexpr, TlError> {
	let exp = match byte {
		b'(' => _parse_sexpr_r(b')', reader, offset)?,
		b'[' => _parse_sexpr_r(b']', reader, offset)?,
		b'#' => {
			let t = _parse_boolean(reader, *offset).map_err(|etype| TlError {etype, offset: *offset})?;
			*offset += 1;
			Sexpr::Atom(t)
		},
		b'"' => {
			let t = _parse_string(reader, *offset)?;
			*offset = t.end;
			Sexpr::Atom(t)
		},
		a if a.is_ascii_digit() => {
			let t = _parse_number(a, reader, *offset)?;
			*offset = t.end;
			Sexpr::Atom(t)
		},
		a => {
			let t = _parse_symbol(a, reader, *offset)?;
			*offset = t.end;
			Sexpr::Atom(t)
		}
	};
	Ok(exp)
}

fn _parse_sexpr_r(term: u8, reader: &mut impl Read, offset: &mut usize) -> Result<Sexpr, TlError> {	// Poor sig. TODO: Re-write structs to have Sexpr offset.
	let mut slist = Vec::new();
	loop {
		let byte = _read_single_u8(reader, *offset)?;
		*offset += 1;
		if byte == term {
			return Ok(Sexpr::List(slist));
		} else if !byte.is_ascii_whitespace() {
			slist.push(_match_rule(byte, reader, offset)?);
		}
	}
}

pub fn parse(reader: &mut impl Read) -> Result<Sexpr, TlError> {
	let mut offset = 0;
	let mut slist = Vec::new();

	loop {
		let byte = _read_single_u8(reader, offset);
		match byte {
			Err(e) => {
				match e.etype {
					TlErrorType::UnexpectedEOF => {
						break;
					},
					_ => {
						return Err(e);
					}
				}
			},
			Ok(byte) => {
				offset += 1;
				if !byte.is_ascii_whitespace() {
					slist.push(_match_rule(byte, reader, &mut offset)?);
				}
			}
		};
	}
	Ok(Sexpr::List(slist))
}