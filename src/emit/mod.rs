mod asm;
mod parser;
mod walker;

/// An aggregate enum with error codes for all errors involved in parsing, and code emitting phases.
#[repr(u8)]
#[derive(Debug)]
pub enum TlErrorType {
	IoError,
	UnexpectedToken,
	UnexpectedEOF,
	InvalidNumeric,
	InvalidChar,
	ReboundName,
	ExpectingList
}

/// A general error type to capture any errors that may occur during translation phase, including parsing and codegen.
pub struct TlError {
	pub(self) etype: TlErrorType,
	pub(self) msg: String,
	pub(self) offset: usize
}

impl TlError {
	fn is_io(&self) -> bool {
		matches!(self.etype, TlErrorType::IoError)
	}
}

impl std::fmt::Display for TlError {
	fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
		if matches!(self.etype, TlErrorType::IoError) {
			write!(fmt, "TlError [{:?}],\n\tdetail: {} (offset: {})", self.etype, self.msg, self.offset)
		} else {
			write!(fmt, "TlError [{:?}] (@{}) : {}", self.etype, self.offset, self.msg)
		}
	}
}

pub use asm::asm;
pub use parser::parse;