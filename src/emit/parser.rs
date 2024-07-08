//! Module for SExpr parser. This parser can handle unicode characters reasonably well.
//! Builds a tree of tokens.

// TODO: Extremely verbose. Desperately needs a re-write.

use std::io::Read;
use crate::utils::CharStream;
use super::{TlError, TlErrorType};

impl TlError {

	pub fn from_ioerr(e: std::io::Error, offset: usize) -> Self {
		TlError { etype: TlErrorType::IoError, msg: format!("{e}"), offset }
	}

	fn eof<T>(stream: &CharStream<'_, T>) -> Self where T: Read {
		TlError { etype: TlErrorType::UnexpectedEOF, msg: "Character stream terminated abruptly.".to_string(), offset: stream.byte_position() }
	}
}

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
	Char(char),
	/// String literal.
	Str(String),
	/// Symbol token.
	Symbol(String)
}

/// Actual token read from input.
/// Contains line number, and offset.
/// Clearly, since offset alone can absolutely locate the token in the stream, line number appears to be redundant, however, it is tracked for debugging purposes.
#[derive(Debug)]
pub struct Token {
	/// The token type.
	ttype: TokenType,
	/// Start offset.
	pub(super) start: usize,
	/// End offset.
	pub(super) end: usize,
	/// The line on which the token occurs.
	pub(super) line: usize
}

macro_rules! atom {
	($variant:ident ($v: expr), $start:expr , $end:expr, $line:expr) => {
		Ok(Sexpr::Atom( Token { ttype: TokenType::$variant($v), start: $start, end: $end, line: $line } ))
	};
}

#[derive(Debug)]
/// Structure for parsing S-expressions.
pub enum Sexpr {
	Atom(Token),
	List(Vec<Sexpr>)
}

const TOKEN_DELIMS: &str = ")(][ \t\n;";

pub fn parse<T>(r: &mut T) -> Result<Sexpr, TlError>
where T: Read {
	let mut stream: CharStream<'_, T> = r.into();
	let mut v = Vec::new();
	loop {
		let ch = stream.skip_whitespace().map_err(|e| TlError::from_ioerr(e, stream.byte_position()))?;
		match ch {
			None => { break; }, // EOF is expected here.
			Some('(') => { v.push(parse_sexpr(&mut stream, ')')?); },
			Some(';') => { parse_comment(&mut stream)?; },
			Some(c) => {
				return Err(TlError { etype: TlErrorType::UnexpectedToken, msg: format!("Root level S-expressions must use only '(', not '{c}'"), offset: stream.byte_position() });
			}
		}
	}
	Ok(Sexpr::List(v))
}

#[inline]
/// Helper function to call `stream.next_char` and map errors to TlErrors.
fn expect_char<T>(stream: &mut CharStream<'_, T>) -> Result<char, TlError>
where T: Read {
	stream.next_char()
		.map_err(|e| TlError::from_ioerr(e, stream.byte_position()))?
		.ok_or_else(|| TlError::eof(stream))
}

/// Fetch characters from the stream and push into buffer, until one in the parameter string is found.
/// Returns the last character read from the stream, or any IoErrors that occured.
/// Returns `Err` if stream reaches EOF.
fn cspan<T>(stream: &mut CharStream<'_, T>, delim: &'static str, buf: &mut String) -> Result<char, TlError>
where T: Read {
	loop {
		let ch = expect_char(stream)?;
		if delim.contains(ch) {
			return Ok(ch);
		}
		buf.push(ch);
	}
}

fn int_from_str_radix_auto(s: &str, offset: usize) -> Result<i64, TlError> {
  let radix = match s.chars().nth(1) {
  	Some('x') => 16,
  	Some('b') => 2,
  	Some('o') => 8,
  	None => {
  		// We already know 0 was the first character.
  		return Ok(0);
  	},
  	Some(c) => {
  		return Err(TlError { etype: TlErrorType::UnexpectedToken, msg: format!("Invalid radix prefix '0{c}'. Expected '0x', '0b', or '0o'."), offset})
  	}
  };
  i64::from_str_radix(&s[2..], radix)
  	.map_err(|e| {
  		let msg = format!("'{s}' is not an integer in base {radix},\n\tdetail: {e}");
  		TlError { etype: TlErrorType::InvalidNumeric, msg, offset }
  	})
}

fn parse_unicode_hex<T>(stream: &mut CharStream<'_, T>) -> Result<char, TlError>
where T: Read {
	let s: Result<String, std::io::Error> = stream.take(4).collect();
	let s = s.map_err(|e| TlError::from_ioerr(e, stream.byte_position()))?;
	if s.len() < 4 {
		Err(TlError { etype: TlErrorType::UnexpectedEOF, msg: format!("Unicode escape requires exactly 4 hex digits, but only '{s}' was provided."), offset: stream.byte_position() })
	} else {
		u32::from_str_radix(&s, 16)
			.map_err(|e| 
				TlError { etype: TlErrorType::InvalidChar, msg: format!("'{s}' is not a valid 4-digit hex for unicode escape sequence.\n\tdetail: {e}"), offset: stream.byte_position() 
			})
			.and_then(|v| {
				char::from_u32(v)
				.ok_or_else(||
					TlError { etype: TlErrorType::InvalidChar, msg: format!("Hex '{s}' does not signify a character."), offset: stream.byte_position() 
				})
			})
	}
}

fn parse_char_raw<T>(stream: &mut CharStream<'_, T>) -> Result<(char, bool), TlError>
where T: Read {
	let ch = expect_char(stream)?;
	if ch != '\\' {
		Ok((ch, false))
	} else {
		let ch = expect_char(stream)?;
		Ok((match ch {
			'\''=> '\'',
			'"' => '"',
			'n' => '\n',
			't' => '\t',
			'r' => '\r',
			'u' => parse_unicode_hex(stream)?,
			a => {
				return Err(TlError { etype: TlErrorType::InvalidChar, msg: format!("Unknown escape sequence \\{a}. Only \\\', \\\",\\r, \\n, \\t, \\uXXXX are allowed."), offset: stream.byte_position() })
			}
		}, true))
	}
}

fn parse_string<T>(stream: &mut CharStream<'_, T>) -> Result<Sexpr, TlError>
where T: Read {
	let (start, start_line) = (stream.byte_position(), stream.lineno());
	
	let mut s = String::new();
	loop {
		let (ch, esc) = parse_char_raw(stream)?;
		if !esc && ch == '\"' {
			break;
		} else {
			s.push(ch);
		}
	}

	atom!(Str(s), start, stream.byte_position(), start_line)
	//Ok(Sexpr::Atom(Token { ttype: TokenType::Str(s), start, end: stream.byte_position() }))
}

fn parse_char<T>(stream: &mut CharStream<'_, T>) -> Result<Sexpr, TlError>
where T: Read {
	let (start, start_line) = (stream.byte_position(), stream.lineno());
	// The single quote has already been read, so..
	let (ch, _esc) = parse_char_raw(stream)?;
	if expect_char(stream)? != '\'' {
		Err(TlError { etype: TlErrorType::InvalidChar, msg: format!("Character literal '{ch}' must be enclosed by single quotes."), offset: stream.byte_position() })
	} else {
		atom!(Char(ch), start, stream.byte_position(), start_line)
	}
}

fn parse_comment<T> (stream: &mut CharStream<'_, T>) -> Result<(), TlError>
where T: Read {
	stream
		.skip_till('\n')
		.map_err(|e| TlError::from_ioerr(e, stream.byte_position()))
		.and_then(|c| {
			if c { Err(TlError::eof(stream)) } else { Ok(()) }
		})
}

fn match_parse_literal (ch: char, buf: String, start: usize, end: usize, lineno: usize) -> Result<Sexpr, TlError> {

	// ---------------------------------------------------- //
	// First match the basic ones. Boolean.

	if buf == "#t" {	// If the whole text is the TRUE boolean literal, then that's the Sexpr.
		atom!(Boolean(true), start, end, lineno)
	} else if buf == "#f" { // Similarly for FALSE.
		atom!(Boolean(false), start, end, lineno)

	// ----------------------------------------------------- //
	// If lead character is alphabetic or '_', then token is a symbol.

	} else if ch.is_alphabetic() || ch == '_' || buf == "+" || buf == "-" { // If the whole token text itself is just '+' or '-', then they are symbols.
		atom!(Symbol(buf), start, end, lineno) // Symbol
	
	// ----------------------------------------------------- //
	// Match integer or floating (numeric) type tokens.

	// If lead character is '0', then the token is either the integer 0, or an integer in hex, binary, or octal
	} else if ch == '0' {
		let value = int_from_str_radix_auto(&buf, start)?;
		return atom!(Integer(value), start, end, lineno);

	// Alternatively, if the lead character is a decimal digit, or '+', '-', the token is of numeric type.
	} else if ch.is_ascii_digit() || ch == '-' || ch == '+' {

		// Attempt to parse text as integer.
		match buf.parse() { 
			Ok(i) => {
				return atom!(Integer(i), start, end, lineno);
			},
			Err(_) => match buf.parse() { // Failed, try to parse as float.
				Ok(f) => { 
					return atom!(Float(f), start, end, lineno);
				},
				Err(_) => {	// This token is neither an integer nor floating point literal.
					return Err(TlError {
						etype: TlErrorType::InvalidNumeric, 
						msg: format!("'{buf}' is neither an integer nor a floating point literal."), 
						offset: start 
					});
				}
			}
		}
	
	// ----------------------------------------------------- //
	// Any other 'ch', i.e, one that is neither alphabetic, _, nor ascii digit, constitutes a symbol.

	} else { 
		// These symbols are no good, so emit a warning/note.
		eprintln!("Note: If token '{buf}' (@ {}) is a symbol, consider renaming with an alphabet or '_' as the first character", start);
		// Ok(Sexpr::Atom(Token { ttype: TokenType::Symbol(buf), start, end}))
		atom!(Symbol(buf), start, end, lineno)
	}
}

fn match_rule<T>(stream: &mut CharStream<'_, T>, v: &mut Vec<Sexpr>, ch: char, close_char: char) -> Result<bool, TlError> 
where T: Read {
	match ch {
		'"' => v.push(parse_string(stream)?),
		// Note: Here, the general 'char literals in single-quotes' convention is preferred over the traditional scheme solution of `#\char`.
		// For our purposes, the special form of quote will not have the shorthand ', so it is reasonable to use the character here.
		'\''=> v.push(parse_char(stream)?),
		'(' => v.push(parse_sexpr(stream, ')')?), '[' => v.push(parse_sexpr(stream, ']')?),
		';' => parse_comment(stream)?,
		_ => {
			let mut buf = String::new(); buf.push(ch);
			
			let start = stream.byte_position()-1;
			let start_line = stream.lineno();
			
			// Keep track of the terminating character, since it can imply another expr, or termination of this one.
			let term = cspan(stream, TOKEN_DELIMS, &mut buf)?;
			
			let end = stream.byte_position()-1;

			// Parse-Match buffer with corresponding literal.
			v.push(match_parse_literal(ch, buf, start, end, start_line)?);			

			if term == close_char {
				return Ok(true); // We're done, this was possibly the last character of the Sexpr.
			} 

			// TODO: Fix ugly nested match arm.

			match term {
				')' | ']' => {
					return Err(TlError {
						etype: TlErrorType::UnexpectedToken, 
						msg: format!("Cannot end token with '{ch}', perhaps you meant '{close_char}'?"), 
						offset: start }
						);
				},
				'(' => v.push(parse_sexpr(stream, ')')?), '[' => v.push(parse_sexpr(stream, ']')?),
				';' => parse_comment(stream)?,
				c if c.is_whitespace() => { /* do nothing */ },
				c => {
					panic!("Token span has ended on a character ({c}) outside TOKEN_DELIMS!");
				}
			}
		}
	}
	Ok(false)
}

fn parse_sexpr<T>(stream: &mut CharStream<'_, T>, close_char: char) -> Result<Sexpr, TlError>
where T: Read {
	let mut v = Vec::new();
	loop {
		let ch = stream.skip_whitespace()
		.map_err(|e| TlError::from_ioerr(e, stream.byte_position()))?
		.ok_or_else(|| TlError::eof(stream))?;
		if ch == close_char { break; }
		if match_rule(stream, &mut v, ch, close_char)? {
			break;
		}
	}
	Ok(Sexpr::List(v))
}