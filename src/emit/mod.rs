mod asm;

use std::ops::Range;
use crate::utils::CharStream;

/// A struct to store location information about a token or list parsed from stream.
/// Contains line number, start and end offsets.
/// Clearly, start offset alone can absolutely locate the token in the stream, and line number appears to be redundant. 
/// Nonetheless, it still is tracked for debugging purposes.
#[derive(Clone, Copy)]
struct TextualLocation {
	/// Offset at which the token or list starts.
	start_offset: usize,
	/// Offset at which the token or list ends.
	end_offset: usize,
	/// Line number associated with this token or list. Usually the line at which it starts.
	line_no: usize
}

impl TextualLocation {
	/// Take the current location from stream.
	fn record<T>(stream: &CharStream<'_, T>) -> TextualLocation 
	where T: std::io::Read {
		let start_offset = stream.byte_position();
		TextualLocation { start_offset, end_offset: start_offset+1, line_no: stream.lineno() }
	}

	/// Returns the location with end offset updated.
	fn end<T>(mut self, stream: &CharStream<'_, T>) -> TextualLocation
	where T: std::io::Read {
		self.end_offset = stream.byte_position();
		self
	}
}

impl core::fmt::Debug for TextualLocation {
	fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
		write!(fmt, "(loc: {}..{} ; {})", self.start_offset, self.end_offset, self.line_no)
	}
}

impl From<&TextualLocation> for Range<usize> {
    fn from(value: &TextualLocation) -> Self {
        value.start_offset..value.end_offset
    }
}

/// Enum for all possible token types.
/// Actual token read from input.
#[derive(Debug, Clone)]
enum Token {
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

impl core::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Token::Integer(v) => state.write_i64(*v),
            Token::Boolean(v) => state.write_u8(*v as u8),
            Token::Float(v) => state.write_u64(v.to_bits()),
            Token::Char(v) => state.write_u32(*v as u32),
            Token::Str(v) => v.hash(state),
            Token::Symbol(v) => v.hash(state),
        }
    }
}

impl PartialEq for Token {

	fn eq(&self, other: &Token) -> bool {
		match (self, other) {
			(Token::Integer(a), Token::Integer(b)) => a.eq(b),
			(Token::Boolean(a), Token::Boolean(b)) => a.eq(b),
			(Token::Char(a), Token::Char(b)) => a.eq(b),
			(Token::Str(a), Token::Str(b)) => a.eq(b),
			(Token::Float(a), Token::Float(b)) => *a == *b,
			(Token::Integer(a), Token::Float(b)) => (*a as f64) == *b,
			(Token::Float(a), Token::Integer(b)) => *a == (*b as f64),
			_ => false
		}
	}
}

impl Eq for Token {}

impl core::fmt::Display for Token {

	fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
		const MAX_STRING_DISPLAY_LEN: usize = 16;

		match self {
			Self::Integer(v) => write!(fmt, "Int({v})"),
			Self::Float(v) => write!(fmt, "Flt({v:?})"),
			Self::Boolean(v) => write!(fmt, "Boolean({v})"),
			Self::Char(v) => write!(fmt, "Chr({v})"),
			Self::Symbol(v) => write!(fmt, "Symbol({v})"),
			Self::Str(s) => {
				if s.len() >= MAX_STRING_DISPLAY_LEN {
					write!(fmt, "Str(\"")?;
					for ch in s.chars().take(MAX_STRING_DISPLAY_LEN) { write!(fmt, "{ch}")?; }
					write!(fmt, "\" ..{} octets)", s.len())
				} else {
					write!(fmt, "Str(\"{s}\")")
				}
			}
		}
	}
}

macro_rules! atom {
	($variant:ident ($v: expr), $loc: expr) => {
		// Ok(Sexpr::Atom( Token { ttype: TokenType::$variant($v), start: $start, end: $end, line: $line } ))
		Ok(crate::emit::Sexpr {
			kind: crate::emit::SexprKind::Atom(
				crate::emit::Token::$variant($v)),
			loc: $loc
		})
	};
	($variant:ident ($v: expr), $start: ident, $end: ident, $lineno: ident) => {
		Ok(crate::emit::Sexpr {
			kind: crate::emit::SexprKind::Atom(
				crate::emit::Token::$variant($v)),
			loc: crate::emit::TextualLocation {
				start_offset: $start,
				end_offset: $end,
				line_no: $lineno
			}
		})
	}
}

macro_rules! list {
	($v: ident, $loc: expr) => {
		Ok(crate::emit::Sexpr {
			loc: $loc,
			kind: crate::emit::SexprKind::List($v)
		})
	};
	($v: ident) => {
		Ok(crate::emit::Sexpr {
			loc: crate::emit::TextualLocation {
				start_offset: 0,
				end_offset: 0,
				line_no: 0
			},
			kind: crate::emit::SexprKind::List($v)
		})
	};
}

/// Structure for S-expressions that are tagged with location.
pub struct Sexpr {
	kind: SexprKind,
	loc: TextualLocation
}

impl core::fmt::Debug for Sexpr {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    	match &self.kind {
    		SexprKind::Atom(t) => {
    			if fmt.alternate() {
    				write!(fmt, "Atom::{} ", t)
    			} else {
    				write!(fmt, "{}", t)
    			}
    		},
    		SexprKind::List(v) => {
    			if fmt.alternate() {
    				writeln!(fmt, "List{v:#?}")
    			} else {
    				write!(fmt, "{v:?}")
    			}
    		}
    	}?;
        write!(fmt, "{:?}", self.loc)
    }
}

/// Enum to represent either a `list`-type S-expression, or an `atom`-type S-expression.
enum SexprKind {
	/// Atom-type S-expression; holds a token.
	Atom(Token),
	/// List-type S-expression; holds many sub-Sexpr, and start offset of this Sexpr.
	List(Vec<Sexpr>)
}

impl Sexpr {
	/// Consume this S-expr and yield the inner list.
	/// Returns `Err` if S-expr is `Atom`.
	fn list(self) -> Result<Vec<Sexpr>, TlError> {
		match self.kind {
			SexprKind::Atom(t) => {
				Err(TlError {
					etype: TlErrorType::ExpectingList,
					msg: format!("Expecting List S-expression, found {t:?}."), 
					loc: self.loc
				})
			},
			SexprKind::List(v) => Ok(v)
		}
	}

	/// Consume this S-expr and yield the inner atom, along with its location.
	/// Returns `Err` if S-expr is `List`.
	fn atom(self) -> Result<(Token, TextualLocation), TlError> {
		match self.kind {
			SexprKind::Atom(t) => Ok((t, self.loc)),
			SexprKind::List(_) => {
				Err(TlError {
					etype: TlErrorType::ExpectingList,
					msg: "Expecting Atom, found list.".to_owned(), 
					loc: self.loc
				})
			}
		}
	}

	/// Get the offset from which this S-expr starts.
	fn location(&self) -> &TextualLocation {
		&self.loc
	}

	/// Iterate through the elements of the S-expr immutably.
	/// Returns `Err` if S-expr is `Atom`.
	fn iter(&self) -> Result<impl Iterator<Item = &Sexpr>, TlError> {
		match &self.kind {
			SexprKind::Atom(t) => {
				Err(TlError {
					etype: TlErrorType::ExpectingList,
					msg: format!("Expecting List S-expression, found {t:?}."), 
					loc: self.loc
				})
			},
			SexprKind::List(v) => {
				Ok(v.iter())
			}
		}
	}

	/// Inspect this S-expr and yield reference to the inner symbol as `&str`.
	/// Returns `Err` if S-expr is a `List`, or any `Atom` other than `Symbol`.
	/// The error comprises of a generic error message, which ought to be augmented with additional context.
	fn inspect_symbol(&self) -> Result<&str, TlError> {
		match &self.kind {
			SexprKind::Atom(Token::Symbol(s)) => Ok(s),
			SexprKind::Atom(t) => {
				Err(TlError {
					etype: TlErrorType::IllegalAtom,
					msg: format!("Expecting Symbol, found {t}."), 
					loc: self.loc
				})
			},
			SexprKind::List(_) => {
				Err(TlError {
					etype: TlErrorType::ExpectingList,
					msg: "Expecting Symbol, found list.".to_owned(), 
					loc: self.loc
				})
			}
		}
	}

	/// Inspect this S-expr and yield reference to inner string.
	/// Returns `Err` if S-expr is a `List` or any `Atom` other than `Symbol`.
	/// The error comprises of a generic error message, which ought to be augmented with additional context.
	fn inspect_str(&self) -> Result<&str, TlError> {
		match &self.kind {
			SexprKind::Atom(Token::Str(s)) => Ok(s),
			SexprKind::Atom(t) => {
				Err(TlError {
					etype: TlErrorType::IllegalAtom,
					msg: format!("Expecting string, found {t}."), 
					loc: self.loc
				})
			},
			SexprKind::List(_) => {
				Err(TlError {
					etype: TlErrorType::ExpectingAtom,
					msg: "Expecting string, found list.".to_string(), 
					loc: self.loc
				})
			}
		}
	}
}

/// An aggregate enum with error codes for all errors involved in parsing, and code emitting phases.
#[derive(Debug)]
pub enum TlErrorType {
	IoError,
	UnexpectedToken,
	UnexpectedEOF,
	InvalidNumeric,
	InvalidChar,
	ReboundName,
	ExpectingList,
	ExpectingAtom,
	UnknownSymbol,
	IllegalAtom,
	InvalidForm,
	EmptyList,
	Complex(Vec<TlError>)
}

/// A general error type to capture any errors that may occur during translation phase, including parsing and codegen.
pub struct TlError {
	etype: TlErrorType,
	msg: String,
	loc: TextualLocation
}

impl TlError {
	fn is_io(&self) -> bool {
		matches!(self.etype, TlErrorType::IoError)
	}

	/// Create a new error whose message has "context: {msg}" appended to it.
	/// This method should be used be used to add more context to an error.
	fn aug(mut self, msg: &str) -> Self {
		self.msg.push_str("\n[*]: ");
		self.msg.push_str(msg);
		self
	}

	fn from_ioerr(e: std::io::Error, offset: usize) -> Self {
		TlError {
			etype: TlErrorType::IoError,
			msg: format!("{e}"), 
			loc: TextualLocation { 
				start_offset: offset,
				end_offset: offset+1,
				line_no: 0
			}
		}
	}

	fn eof<T>(stream: &CharStream<'_, T>) -> Self
	where T: std::io::Read {
		TlError {
			etype: TlErrorType::UnexpectedEOF,
			msg: "Character stream terminated abruptly.".to_string(),
			loc: TextualLocation::record(stream) 
		}
	}

	/// Collect multiple errors (if any) into a single `Complex` error.
	/// Returns `Ok(())` if none of the results yielded by iterator are errors.
	fn collect(itr: impl Iterator<Item = Result<(), TlError>>, msg: impl Into<String>) -> Result<(), TlError> {
		let mut v = Vec::new();
		for it in itr {
			if let Err(e) = it { v.push(e) }
		}
		if v.is_empty() {
			Ok(())
		} else {
			let loc = v[0].loc;
			Err(TlError {
				etype: TlErrorType::Complex(v),
				msg: msg.into(),
				loc
			})
		}
	}
}

impl std::fmt::Debug for TlError {
	fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
		match &self.etype {
			TlErrorType::IoError => write!(fmt, "TlError [{:?}], {:?}\n\tdetail: {}", self.etype, self.loc, self.msg),
			TlErrorType::Complex(v) if fmt.alternate() => {
				writeln!(fmt, "TlError [Complex]: {}, {:?}", self.msg, self.loc)?;
				for i in v.iter() {
					writeln!(fmt, "|-----> {i:#?}")?;
				}
				Ok(())
			},
			_ => write!(fmt, "TlError [{:?}]: {} {:?}", self.etype, self.msg, self.loc)
		}
	}
}

mod parser;
mod walker;

pub use asm::asm;
pub use parser::parse;
pub use walker::walk;