//! Module for SExpr parser. This parser does not handle unicode characters. ASCII only.
//! Builds a tree of tokens.

// TODO: Extremely verbose. Desperately needs a re-write.

use std::io::Read;
use crate::utils::CharStream;

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

/// An aggregate enum with error codes for all errors involved in parsing, and code emitting phases.
#[repr(u8)]
#[derive(Debug)]
pub enum TlErrorType {
	IoError,
	UnexpectedToken,
	UnexpectedEOF,
	InvalidNumeric,
	InvalidToken,
	InvalidBoolean
}

pub struct TlError {
	etype: TlErrorType,
	msg: String,
	offset: usize
}

impl std::fmt::Display for TlError {
	fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
		write!(fmt, "TlError [{:?}] (@{}) : {}", self.etype, self.offset, self.msg)
	}
}

impl TlError {
	fn from_ioerr(e: std::io::Error, offset: usize) -> Self {
		TlError { etype: TlErrorType::IoError, msg: format!("{e}"), offset }
	}

	fn eof<T>(stream: &CharStream<'_, T>) -> Self where T: Read {
		TlError { etype: TlErrorType::UnexpectedEOF, msg: "Character stream terminated abruptly.".to_string(), offset: stream.byte_position() }
	}

	fn is_eof(&self) -> bool {
		matches!(self.etype, TlErrorType::UnexpectedEOF)
	}
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

/// Fetch characters from the stream and push into buffer, until one in the parameter string is found.
/// Returns the last character read from the stream, or any IoErrors that occured.
/// Returns `Err` if stream reaches EOF.
fn cspan<T>(stream: &mut CharStream<'_, T>, delim: &'static str, buf: &mut String) -> Result<char, TlError>
where T: Read {
	loop {
		let ch = stream.next_char()
			.map_err(|e| TlError::from_ioerr(e, stream.byte_position()))?
			.ok_or_else(|| TlError::eof(stream))?;
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
  i64::from_str_radix(s.trim_start_matches(['0', 'x', 'o', 'b']), radix)
  	.map_err(|e| {
  		let msg = format!("'{s}' is not an integer in base {radix},\n\tdetail: {e}");
  		TlError { etype: TlErrorType::InvalidNumeric, msg, offset }
  	})
}

fn parse_string<T>(_stream: &mut CharStream<'_, T>) -> Result<Sexpr, TlError>
where T: Read {
	todo!("Implement a string parser, with escape-sequence translation.")
}

fn parse_char<T>(_stream: &mut CharStream<'_, T>) -> Result<Sexpr, TlError>
where T: Read {
	todo!("Parse a single character with escape-sequence translation.")
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

fn match_parse_literal (ch: char, buf: String, start: usize, end: usize) -> Result<Sexpr, TlError> {
	// First match the basic ones
	if buf == "#t" {	// If the whole text is the TRUE boolean literal, then that's the Sexpr.
		Ok(Sexpr::Atom(Token { ttype: TokenType::Boolean(true), start, end}))
	} else if buf == "#f" { // Similarly for FALSE.
		Ok(Sexpr::Atom(Token { ttype: TokenType::Boolean(false), start, end }))
	// If lead character is alphabetic or '_', then token is a symbol.
	} else if ch.is_alphabetic() || ch == '_' || buf == "+" || buf == "-" { // If the whole token text itself is just '+' or '-', then they are symbols.
		Ok(Sexpr::Atom(Token { ttype: TokenType::Symbol(buf), start, end})) // Symbol
	// If lead character is '0', then the token is either the integer 0, or an integer in hex, binary, or octal
	} else if ch == '0' {
		let value = int_from_str_radix_auto(&buf, start)?;
		return Ok(Sexpr::Atom(Token { ttype: TokenType::Integer(value), start, end}));
	// Alternatively, if the lead character is a decimal digit, or '+', '-', the token is of numeric type.
	} else if ch.is_ascii_digit() || ch == '-' || ch == '+' {
		// Attempt to parse text as integer.
		match buf.parse() { Ok(i) => { return Ok(Sexpr::Atom(Token { ttype: TokenType::Integer(i), start, end}));},
			Err(_) => match buf.parse() { // Attempt to parse as float.
				Ok(f) => { return Ok(Sexpr::Atom(Token { ttype: TokenType::Float(f), start, end}));},
				Err(_) => {	// This token is neither an integer nor floating point.
					return Err(TlError { etype: TlErrorType::InvalidNumeric, msg: format!("'{buf}' is neither an integer nor a floating point literal."), offset: start });
				}
			}
		}
	} else { // Any other 'ch', i.e, one that is neither alphabetic, _, nor ascii digit, constitutes a symbol.
		// These symbols are not good, so emit a warning/note.
		eprintln!("Note: If token '{buf}' (@ {}) is a symbol, consider starting with an alphabet or '_'", start);
		Ok(Sexpr::Atom(Token { ttype: TokenType::Symbol(buf), start, end}))
	}
}

fn match_rule<T>(stream: &mut CharStream<'_, T>, v: &mut Vec<Sexpr>, ch: char, close_char: char) -> Result<bool, TlError> 
where T: Read {
	match ch {
		'\'' => v.push(parse_char(stream)?),
		'"' => v.push(parse_string(stream)?),
		'(' => v.push(parse_sexpr(stream, ')')?),
		'[' => v.push(parse_sexpr(stream, ']')?),
		';' => parse_comment(stream)?,
		_ => {
			let mut buf = String::new(); buf.push(ch);
			
			let start = stream.byte_position()-1;
			
			// Keep track of the terminating character, since it can imply another expr, or termination of this one.
			let term = cspan(stream, TOKEN_DELIMS, &mut buf)?;
			
			let end = stream.byte_position()-1;

			// Parse-Match buffer with corresponding literal.
			v.push(match_parse_literal(ch, buf, start, end)?);			

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
				'(' => v.push(parse_sexpr(stream, ')')?),
				'[' => v.push(parse_sexpr(stream, ']')?),
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