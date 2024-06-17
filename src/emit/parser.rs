//! Module for SExpr parser. This parser does not handle unicode characters. ASCII only.
//! Builds a tree of tokens.

use std::io::Read;

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
struct Token {
	/// The token type.
	ttype: TokenType,
	/// Start offset.
	start: usize,
	/// End offset.
	end: usize
}

#[derive(Debug)]
/// Structure for parsing S-expressions.
enum Sexpr {
	Atom(Token),
	List(Vec<Sexpr>)
}

/*
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
}*/

/// An aggregate enum with error codes for all errors involved in parsing, and code emitting phases.
#[repr(u8)]
pub enum TlError {
	IoError,
	UnexpectedToken,
	UnexpectedEOF,
	TokenTooBig,
	InvalidNumeric,
	InvalidBoolean
}

const NUMERIC_LITERAL_MAX_CHARS: usize = 48;
const SYMBOL_MAXW_CHARS: usize = 56;
const STRING_LITERAL_MAXW_CHARS: usize = 2048;

fn _parse_number(lead: u8, reader: &mut impl Read, offset: usize) -> Result<Token, (TlError, usize)> {
	// Ugly, but works I suppose.
	// TODO: Fix this, if possible.

	let mut charbuffer = [0u8; NUMERIC_LITERAL_MAX_CHARS];
	let mut p = 1;	// Definitely not very Rust-like. 
	
	let mut is_float = false;

	charbuffer[0] = lead;
	for byte in reader.bytes() {
		if p >= NUMERIC_LITERAL_MAX_CHARS {
			return Err((TlError::TokenTooBig, offset + p));
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
					return Err((TlError::InvalidNumeric, offset + p));
				}
				p += 1;
			},
			Err(e) => {
				eprintln!("IoError while reading byte. {e:?}");
				return Err((TlError::IoError, offset + p));
			}
		}
	}

	let s = core::str::from_utf8(&charbuffer[..p]).expect("Numeric type parser does not have valid charbuffer.");
	if is_float {
		let v: f64 = s.parse().map_err(|_| (TlError::InvalidNumeric, p))?;
		Ok(Token { ttype: TokenType::Float(v), start: offset, end: offset + p })
	} else {
		let v: i64 = s.parse().map_err(|_| (TlError::InvalidNumeric, p))?;
		Ok(Token { ttype: TokenType::Integer(v), start: offset, end: offset + p })
	}
}

fn _parse_boolean(reader: &mut impl Read, offset: usize) -> Result<Token, TlError> {
	let nch = reader.bytes().next().ok_or(TlError::UnexpectedEOF)?.map_err(|e| {
		eprintln!("IoError while reading byte. {e:?}");
		TlError::IoError
	})?;
	match nch {
		b't' => Ok(Token { ttype: TokenType::Boolean(true), start: offset, end: offset + 2 }),
		b'f' => Ok(Token { ttype: TokenType::Boolean(false), start: offset, end: offset + 2 }),
		_ => Err(TlError::InvalidBoolean)
	}
}

fn _parse_symbol(lead: u8, reader: &mut impl Read, offset: usize) -> Result<Token, (TlError, usize)> {
	let mut p = 1;
	let mut s = String::with_capacity(SYMBOL_MAXW_CHARS >> 1);
	s.push(lead as char);
	for byte in reader.bytes() {
		match byte {
			Err(e) => {
				eprintln!("IoError while reading byte. {e:?}");
				return Err((TlError::IoError, offset + p));
			},
			Ok(u) => {
				if u.is_ascii_whitespace() { break; }
				s.push(u as char);
				p += 1;
			}
		}
	}
	Ok(Token { ttype: TokenType::Symbol(s), start: offset, end: offset + p})
}