use std::ops::Rem;
use std::ops::Mul;
use std::ops::Sub;
use std::ops::Add;

use core::fmt::Formatter;
use core::fmt::Display;

use crate::exec::RefCounter;

#[derive(Debug)]
#[derive(Clone)]
pub enum ErrorType {
	ModuleLoadFailure,
	UtfDecodeError,
	InvalidOpcode,
	InvalidRegister,
	EmptyRegister,
	IncompleteInstr,
	TypeMismatch,
	ZeroDivision,
	MutableAliasing,
	CoincidentRef,
	NoRefsToDec,
	PrematureDealloc,
	NoSuchFunction,
	NoSuchModule,
	NotCallable,
	InvalidCall,
	AssertionFailure,
	ParsingFailure,
	NullPointer,
	NativeError
}

#[derive(Debug, Clone)]
pub struct FatalErr {
	error_t: ErrorType,
	msg: String
}

pub fn new_error(errt: ErrorType, msg: impl Into<String>) -> FatalErr {
	FatalErr {
		error_t: errt,
		msg: msg.into()
	}
}

impl Display for FatalErr {
	fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(fmt, "Error [{:?}]: {}", self.error_t, self.msg)
	}
}

/// Enum for dynamically sized types.
#[derive(Debug)]
pub enum CompositeType {
	Str(String),
	// List(Vec<BaseType>), // TODO: Implement list operations.
	// Table(HashMap<String, BaseType>), // TODO: Sort out details of dict implementation. 
	FRef {
		mod_id: usize,
		func_idx: usize,
		unext: bool
	},
}

impl CompositeType {
	pub fn new_fref(modid: usize, fid: usize, ext: bool) -> BaseType {
		BaseType::Alloc(Box::new(CompositeType::FRef{mod_id: modid, func_idx: fid, unext: ext}))
	}

	pub fn lt(&self, other: &CompositeType) -> Result<BaseType, FatalErr> {
		match (self, other) {
			(CompositeType::Str(s1), CompositeType::Str(s2)) => Ok(BaseType::Int((s1 < s2) as i64)),
			_ => Err(new_error(ErrorType::TypeMismatch, format!("Cannot compare {self:?} and {other:?}")))
		}
	}

	pub fn gt(&self, other: &CompositeType) -> Result<BaseType, FatalErr> {
		match (self, other) {
			(CompositeType::Str(s1), CompositeType::Str(s2)) => Ok(BaseType::Int((s1 > s2) as i64)),
			_ => Err(new_error(ErrorType::TypeMismatch, format!("Cannot compare {self:?} and {other:?}")))
		}
	}

	pub fn eq(&self, other: &CompositeType) -> Result<BaseType, FatalErr> {
		match (self, other) {
			(CompositeType::Str(s1), CompositeType::Str(s2)) => Ok(BaseType::Int((s1 == s2) as i64)),
			_ => Err(new_error(ErrorType::TypeMismatch, format!("Cannot compare {self:?} and {other:?}")))
		}
	}

	pub fn length(&self) -> Result<BaseType, FatalErr> {
		match self {
			CompositeType::Str(s) => {
				Ok(BaseType::Int(s.len() as i64))
			},
			_ => Err(new_error(ErrorType::TypeMismatch, format!("Unsupported 'length' on {self:?}")))
		}
	}
}

impl Display for CompositeType {
	fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
		match self {
			CompositeType::Str(s) => write!(fmt, "{}", s),
			a => write!(fmt, "{:?}", a)
		}
	}
}

/// Enum for fixed-size types.
#[derive(Debug)]
pub enum BaseType {
	Int(i64),
	Flt(f64),
	Chr(char),
	/// Owned allocation to a heap object.
	Alloc(Box<CompositeType>),
	/// Immutable pointer to an object.
	ConstRef(*const CompositeType),
	/// Mutable pointer to an object.
	MutRef(*mut CompositeType),
	/// Unsigned pointer-width integer, intended for native APIs to use to return opaque handles to objects.
	OpaqueHandle(usize)
}

fn ref_to_addr<T>(r: &T) -> usize {
	(r as *const T) as usize
}

impl PartialEq for BaseType {
	fn eq(&self, other: &BaseType) -> bool {
		match (self, other) {
			(BaseType::Int(i1), BaseType::Int(i2)) => i1 == i2,
			(BaseType::Int(i1), BaseType::Flt(f2)) => *i1 as f64 == *f2,
			(BaseType::Flt(f2), BaseType::Int(i1)) => *i1 as f64 == *f2,
			(BaseType::Flt(f1), BaseType::Flt(f2)) => f1 == f2,
			(BaseType::Chr(c1), BaseType::Chr(c2)) => c1 == c2,
			(BaseType::Alloc(a1), BaseType::Alloc(a2)) => ref_to_addr(a1.as_ref()) == ref_to_addr(a2.as_ref()),
			(BaseType::ConstRef(c1), BaseType::ConstRef(c2)) => c1 == c2,
			(BaseType::MutRef(c1), BaseType::MutRef(c2)) => c1 == c2,
			(BaseType::OpaqueHandle(h1), BaseType::OpaqueHandle(h2)) => h1 == h2,
			_ => false
		}
	}
}

impl Display for BaseType {

	fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
		match self {
			BaseType::Int(i1) => write!(fmt, "{i1}"),
			BaseType::Flt(f1) => write!(fmt, "{f1}"),
			BaseType::Chr(ch) => write!(fmt, "{ch}"),
			BaseType::Alloc(a1) => write!(fmt, "{a1:?}"),
			BaseType::ConstRef(c1) => write!(fmt, "<&{:x}>", *c1 as usize),
			BaseType::MutRef(c1) => write!(fmt, "<&mut{:x}>", *c1 as usize),
			BaseType::OpaqueHandle(h1) => write!(fmt, "<ptr@{:x}>", *h1 as usize)	
		}
	}
}

macro_rules! impl_arith_op (
	($fn_name: ident, $e: ident, $och: literal) => {
		pub fn $fn_name (v1: &BaseType, v2: &BaseType) -> Result<BaseType, FatalErr> {
			match (v1, v2) {
				(BaseType::Int(a), BaseType::Int(b)) => Ok(BaseType::Int(i64::$e(*a,b))),
				(BaseType::Flt(a), BaseType::Flt(b)) => Ok(BaseType::Flt(f64::$e(*a,b))),
				(BaseType::Int(a), BaseType::Flt(b)) => Ok(BaseType::Flt(f64::$e(*a as f64, b))),
				(BaseType::Flt(a), BaseType::Int(b)) => Ok(BaseType::Flt(f64::$e(*a, *b as f64))),
				_ => Err(new_error(ErrorType::TypeMismatch, format!("Unsupported {} between {v1}, {v2}", $och)))
			}
		}
	};
);

pub fn try_exp(v1: &BaseType, v2: &BaseType) -> Result<BaseType, FatalErr> {
	match (v1, v2) {
		(BaseType::Int(a), BaseType::Int(b)) => {
			if *b < 0 {
				Ok(BaseType::Flt(f64::powf(*a as f64, *b as f64)))
			} else {
				Ok(BaseType::Int(i64::checked_pow(*a, *b as u32).unwrap_or(i64::MAX)))
			}
		},
		(BaseType::Flt(a), BaseType::Int(b)) => Ok(BaseType::Flt(f64::powf(*a, *b as f64))),
		(BaseType::Int(a), BaseType::Flt(b)) => Ok(BaseType::Flt(f64::powf(*a as f64, *b))),
		(BaseType::Flt(a), BaseType::Flt(b)) => Ok(BaseType::Flt(f64::powf(*a,*b))),
		_ => Err(new_error(ErrorType::TypeMismatch, format!("Could not compute {v1} raised to {v2}")))
	}
}

pub fn try_floor(v1: &BaseType) -> Option<BaseType> {
	match v1 {
		BaseType::Int(i1) => Some(BaseType::Int(*i1)),
		BaseType::Flt(f1) => Some(BaseType::Int(f64::floor(*f1) as i64)),
		_ => None
	}
}

pub fn try_idiv(v1: &BaseType, v2: &BaseType) -> Result<BaseType, FatalErr> {
	let i1 = match v1 {
		BaseType::Int(i1) => *i1,
		BaseType::Flt(f1) => *f1 as i64,
		_ => {
			return Err(new_error(ErrorType::TypeMismatch, format!("Unsupported '//' between {v1}, {v2}")));
		}
	};
	let i2 = match v2 {
		BaseType::Int(i1) => *i1,
		BaseType::Flt(f1) => *f1 as i64,
		_ => {
			return Err(new_error(ErrorType::TypeMismatch, format!("Unsupported '//' between {v1}, {v2}")));
		}
	};
	if i2 == 0 {
		return Err(new_error(ErrorType::ZeroDivision, format!("Cannot divide by zero")));
	}
	Ok(BaseType::Int(i1 / i2))
}

pub fn try_div(v1: &BaseType, v2: &BaseType) -> Result<BaseType, FatalErr> {
	let i1 = match v1 {
		BaseType::Int(i1) => *i1 as f64,
		BaseType::Flt(f1) => *f1 as f64,
		_ => {
			return Err(new_error(ErrorType::TypeMismatch, format!("Unsupported '/' between {v1}, {v2}")));
		}
	};
	let i2 = match v2 {
		BaseType::Int(i1) => *i1 as f64,
		BaseType::Flt(f1) => *f1 as f64,
		_ => {
			return Err(new_error(ErrorType::TypeMismatch, format!("Unsupported '/' between {v1}, {v2}")));
		}
	};
	if i2 == 0.0 {
		return Err(new_error(ErrorType::ZeroDivision, format!("Cannot divide by zero")));
	}
	Ok(BaseType::Flt(i1 / i2))
}

#[inline]
pub fn try_bitwise_and(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
	match (rv1, rv2) {
		(BaseType::Int(a), BaseType::Int(b)) => Ok(BaseType::Int(*a & *b)),
		_ => {return Err(new_error(ErrorType::TypeMismatch, format!("Unsupported '&' between {rv1}, {rv2}")));}
	}
}

#[inline]
pub fn try_bitwise_or(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
	match (rv1, rv2) {
		(BaseType::Int(a), BaseType::Int(b)) => Ok(BaseType::Int(*a | *b)),
		_ => {return Err(new_error(ErrorType::TypeMismatch, format!("Unsupported '|' between {rv1}, {rv2}")));}
	}
}

#[inline]
pub fn try_left_bitshift(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
	match (rv1, rv2) {
		(BaseType::Int(a), BaseType::Int(b)) => Ok(BaseType::Int(*a << *b)),
		_ => {return Err(new_error(ErrorType::TypeMismatch, format!("Unsupported '<<' between {rv1}, {rv2}")));}
	}
}

#[inline]
pub fn try_right_bitshift(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
	match (rv1, rv2) {
		(BaseType::Int(a), BaseType::Int(b)) => Ok(BaseType::Int(*a >> *b)),
		_ => {return Err(new_error(ErrorType::TypeMismatch, format!("Unsupported '>>' between {rv1}, {rv2}")));}
	}
}

#[inline]
pub fn try_bitwise_xor(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
	match (rv1, rv2) {
		(BaseType::Int(a), BaseType::Int(b)) => Ok(BaseType::Int(*a ^ *b)),
		_ => {return Err(new_error(ErrorType::TypeMismatch, format!("Unsupported '^' between {rv1}, {rv2}")));}
	}
}

macro_rules! impl_rel_op {
	($fname: ident, $func: ident, $ch: literal) => {
		pub fn $fname (rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
			match (rv1, rv2) {
				(BaseType::Int(i1), BaseType::Int(i2)) => Ok(BaseType::Int(i1.$func(i2) as i64)),
				(BaseType::Flt(f1), BaseType::Flt(f2)) => Ok(BaseType::Int(f1.$func(f2) as i64)),
				(BaseType::Flt(f1), BaseType::Int(i2)) => Ok(BaseType::Int(f1.$func(&(*i2 as f64)) as i64)),
				(BaseType::Int(i1), BaseType::Flt(f2)) => Ok(BaseType::Int((*i1 as f64).$func(f2) as i64)),
				(BaseType::Chr(c1), BaseType::Chr(c2)) => Ok(BaseType::Int(c1.$func(c2) as i64)),
				(BaseType::Alloc(a1), BaseType::Alloc(a2)) => {a1.lt(a2)},
				(BaseType::ConstRef(a1), BaseType::ConstRef(a2)) => unsafe {
					let a1 = a1.as_ref().unwrap();
					let a2 = a2.as_ref().unwrap();
					a1.$func(a2)
				},
				_ => Err(new_error(ErrorType::TypeMismatch, format!("Unsupported \'{}\' between {rv1}, {rv2}", $ch)))
			}
		}
	};
}

impl_arith_op!(try_add, add, '+');
impl_arith_op!(try_mul, mul, '*');
impl_arith_op!(try_sub, sub, '-');
impl_arith_op!(try_rem, rem, '%');
impl_rel_op!(try_lt, lt, '<');
impl_rel_op!(try_gt, gt, '>');
impl_rel_op!(try_eq, eq, "==");

#[inline]
pub fn try_neq(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
	match try_eq(rv1, rv2) {
		Ok(b) => {
			if let BaseType::Int(i1) = b {
				Ok(BaseType::Int(1-i1))
			} else {
				Ok(BaseType::Int(0))
			}
		},
		_ => Ok(BaseType::Int(0))
	}
}

macro_rules! impl_neg_op {
	($fname: ident, $cfunc: ident, $ch: literal) => {
		#[inline]
		pub fn $fname(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
			match $cfunc(rv1, rv2) {
				Ok(b) => {
					if let BaseType::Int(i1) = b {
						Ok(BaseType::Int(1-i1))
					} else {
						Ok(BaseType::Int(0))
					}
				},
				_ => Err(new_error(ErrorType::TypeMismatch, format!("Unsupported \'{}\' between {rv1}, {rv2}", $ch)))
			}
		}
	};
}

impl_neg_op!(try_lte, try_gt, "<=");
impl_neg_op!(try_gte, try_lt, ">=");

#[inline]
pub fn try_lnot(rv1: &BaseType) -> Result<BaseType, FatalErr> {
	match rv1 {
		BaseType::Int(i1) => {
			if *i1 != 0 {
				Ok(BaseType::Int(0))
			} else {
				Ok(BaseType::Int(1))
			}
		},
		_ => Err(new_error(ErrorType::TypeMismatch, format!("Unsupported 'logical NOT' on {rv1}")))
	}
}

#[inline]
pub fn as_bool(rv: &BaseType) -> bool {
	match rv {
		BaseType::Int(i1) => {*i1 != 0},
		BaseType::Flt(f1) => {*f1 != 0.0},
		BaseType::Chr(c1) => {*c1 != '\0'},
		_ => true
	}
}

pub(crate) fn as_composite_type<'a>(rv: &'a BaseType, refc: &RefCounter) -> Result<&'a CompositeType, FatalErr> {
	let ret = match rv {
		BaseType::Alloc(a) => {
			a.as_ref()
		},
		BaseType::ConstRef(ptr) => {
			unsafe{ ptr.as_ref().ok_or(new_error(ErrorType::NullPointer, format!("{rv} is NULL and cannot be referenced."))) }?
		},
		_ => { return Err(new_error(ErrorType::TypeMismatch, format!("{rv} is NOT a composite type."))) }
	};
	refc.verify_borrow(ret)?;
	Ok(ret)
}

pub(crate) fn as_composite_type_mut<'a>(rv: &'a mut BaseType, refc: &RefCounter) -> Result<&'a mut CompositeType, FatalErr> {
	match rv {
		BaseType::Alloc(a) => {
			let mref = a.as_mut();
			refc.verify_borrow_mut(mref)?;
			Ok(mref)
		},
		BaseType::MutRef(ptr) => {
			Ok(unsafe { ptr.as_mut().ok_or(new_error(ErrorType::NullPointer, format!("{rv} is NULL and cannot be referenced.")))}?)
		},
		_ => { Err(new_error(ErrorType::TypeMismatch, format!("{rv} is NOT a composite type."))) }
	}
}

#[inline]
pub fn as_string(rv: &BaseType) -> Result<String,FatalErr> {
	Ok(match rv {
		BaseType::ConstRef(ptr) => {
			let ctype = unsafe { ptr.as_ref().ok_or(new_error(ErrorType::NullPointer, format!("{rv} is NULL and cannot be referenced.")))}?;
			format!("{ctype}")
		},
		_ => format!("{rv}")
	})
}