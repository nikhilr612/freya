use std::ops::Rem;
use std::ops::Mul;
use std::ops::Sub;
use std::ops::Add;
use std::iter::zip;
use core::fmt::Formatter;
use core::fmt::Display;
use std::cmp::Ordering;
use crate::exec::RefCounter;

#[derive(Debug)]
#[derive(Clone)]
pub enum ErrorType {
	ModuleLoadFailure,
	AssertionFailure,
	ParsingFailure,
	UtfDecodeError,
	InvalidOpcode,
	InvalidRegister,
	IllegalState,
	IllegalValue,
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
	NullPointer,
	InvalidIndex,
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
	Slice {
		parent: *const CompositeType,
		start_off: usize,
		end_off: usize,
		reverse: bool
	},
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

	fn cmp(&self, other: &CompositeType) -> Result<Ordering, FatalErr> {
		match (self, other) {
			(CompositeType::Str(s1), CompositeType::Str(s2)) => Ok(s1.cmp(s2)),
			(CompositeType::Slice { parent: p1, start_off: s_off1, end_off: e_off1, reverse: rev1 }, CompositeType::Slice { parent: p2, start_off: s_off2, end_off: e_off2, reverse: rev2 }) => {
				let p1 = unsafe{ p1.as_ref().ok_or(new_error(ErrorType::NullPointer, format!("Slice is over a NULL parent."))) }?;
				let p2 = unsafe{ p2.as_ref().ok_or(new_error(ErrorType::NullPointer, format!("Slice is over a NULL parent."))) }?;
				match (p1, p2) {
					(CompositeType::Str(s1), CompositeType::Str(s2)) => {
						let slice1 = &s1[*s_off1..*e_off1];
						let slice2 = &s2[*s_off2..*e_off2];
						Ok(order_with_rev(slice1.chars(), slice2.chars(), *rev1, *rev2)?.unwrap_or(slice1.len().cmp(&slice2.len())))
					},
					_ => Err(new_error(ErrorType::TypeMismatch, format!("Unsupported comparison between {p1}, {p2}")))
				}
			},
			_ => Err(new_error(ErrorType::TypeMismatch, format!("Cannot compare {self:?} and {other:?}")))
		}
	}

	pub fn length(&self) -> Result<BaseType, FatalErr> {
		match self {
			CompositeType::Str(s) => {
				Ok(BaseType::Int(s.len() as i64))
			},
			CompositeType::Slice{parent: _, start_off, end_off, ..} => {
				Ok(BaseType::Int((end_off - start_off) as i64))
			},
			CompositeType::FRef {..} => Err(new_error(ErrorType::TypeMismatch, format!("Unsupported 'length' on {self:?}")))
		}
	}

	pub fn push(&mut self, val: BaseType) -> Result<(), FatalErr> {
		match self {
			CompositeType::Str(s) => Ok(s.push_str(&as_printrepr(&val)?)),
			CompositeType::FRef {..} | CompositeType::Slice {..} => {
				Err(new_error(ErrorType::TypeMismatch, format!("Unsupported 'push' on {self:?}")))
			}
		}
	}

	pub fn pop(&mut self) -> Result<BaseType, FatalErr> {
		match self {
			CompositeType::Str(s) => {
				let ch = s.pop();
				match ch {
					None => Err(new_error(ErrorType::IllegalState, format!("Cannot 'pop' from empty string."))),
					Some(ch) => Ok(BaseType::Chr(ch)),
				}
			},
			CompositeType::Slice { parent, start_off, end_off, reverse } => {
				let parent = unsafe{ parent.as_ref().ok_or(new_error(ErrorType::NullPointer, format!("Slice is over a NULL parent."))) }?;
				match parent {
					CompositeType::Str(string) => {
						let stringref = &string[*start_off..*end_off];
						if *reverse {
							let ch = stringref.chars().next().ok_or(new_error(ErrorType::IllegalState, format!("Cannot 'pop' from empty slice.")))?;
							*start_off += ch.len_utf8();
							Ok(BaseType::Chr(ch))
						} else {
							let (index, ch) = stringref.char_indices().next_back().ok_or(new_error(ErrorType::IllegalState, format!("Cannot 'pop' from empty slice.")))?;
							*end_off = index + *start_off;
							Ok(BaseType::Chr(ch))
						}
					},
					// TODO: Add List Slices.
					CompositeType::Slice {..} | CompositeType::FRef {..} => panic!("Slices cannot have any parents other than Str or List.")
				}
			}
			CompositeType::FRef {..} => {
				Err(new_error(ErrorType::TypeMismatch, format!("Unsupported 'pop' on {self:?}")))
			}
		}
	}

	fn _partial_verify_index(idx: &BaseType, len: usize) -> Result<usize, FatalErr> {
		let idx = match idx {
			BaseType::Int(i) => *i,
			a => {return Err(new_error(ErrorType::TypeMismatch, format!("Cannot index with value {a}, must be Int")));}
		};
		let idx = if idx < 0 { idx + len as i64 } else {idx};
		if idx < 0 {
			return Err(new_error(ErrorType::InvalidIndex, format!("{idx}  < -{len}. Index is atleast -length")));
		}
		Ok(idx as usize)
	}

	fn _verify_index(idx: &BaseType, len: usize) -> Result<usize, FatalErr> {
		let idx = Self::_partial_verify_index(idx, len)?;
		if idx >= len {
			return Err(new_error(ErrorType::InvalidIndex, format!("{idx} >= {len}. Index is strictly less than length.")))
		};
		Ok(idx)
	}

	fn _new_slice(s_off: &BaseType, e_off: &BaseType, len: usize) -> Result<(usize, usize, bool), FatalErr> {
		let s_off = Self::_partial_verify_index(s_off, len).unwrap_or(0).min(len);
		let e_off = Self::_partial_verify_index(e_off, len).unwrap_or(0).min(len);
		if s_off > e_off {
			Ok((e_off, s_off, true))
		} else {
			Ok((s_off, e_off, false))
		}
	}

	pub(crate) fn slice(&self, s1: &BaseType, s2: &BaseType, refc: &mut RefCounter) -> Result<BaseType, FatalErr> {
		match self {
			CompositeType::Str(s) => {
				let (s_off, e_off, rev) = Self::_new_slice(s1, s2, s.len())?;
				let parent = self as *const CompositeType;
				let ctype = CompositeType::Slice {parent, start_off: s_off, end_off: e_off, reverse: rev};
				refc.incref(parent)?;
				Ok(BaseType::Alloc(Box::new(ctype)))
			},
			CompositeType::Slice{ parent, start_off, end_off, reverse } => {
				let len = end_off - start_off;
				refc.incref(*parent)?;
				let (s_off, e_off, rev) = Self::_new_slice(s1, s2, len)?;
				let ctype = if !reverse {
					let real_s_off = start_off + s_off;
					let real_e_off = start_off + e_off;
					CompositeType::Slice {parent: *parent, start_off: real_s_off, end_off: real_e_off, reverse: rev}
				} else {
					let real_s_off = end_off - s_off;
					let real_e_off = end_off - e_off;
					CompositeType::Slice {parent: *parent, start_off: real_e_off, end_off: real_s_off, reverse: !rev}
				};
				Ok(BaseType::Alloc(Box::new(ctype)))
			},
			a => Err(new_error(ErrorType::TypeMismatch, format!("Cannot slice value {a}")))
		}
	}

	fn  _get_index_unchecked(&self, index: usize) -> BaseType {
		match self {
			CompositeType::Str(s) => BaseType::Chr((s.as_bytes())[index] as char),
			_ => panic!("Non-indexable composite-type somehow became parent of slice")
		}
	}

	pub fn getindex(&self, idx: &BaseType) -> Result<BaseType, FatalErr> {
		match self {
			CompositeType::Str(s) => {
				let index = Self::_verify_index(idx, s.len())?;
				Ok(BaseType::Chr((s.as_bytes())[index] as char))
			},
			CompositeType::Slice { parent, start_off, end_off, reverse } => {
				let index  = Self::_verify_index(idx, end_off - start_off)?;
				let parent = unsafe{ parent.as_ref().ok_or(new_error(ErrorType::NullPointer, format!("{self} is NULL and cannot be referenced."))) }?;
				if *reverse {
					Ok(parent._get_index_unchecked(end_off - index))
				} else {
					Ok(parent._get_index_unchecked(start_off + index))
				}
			},
			_ => {
				Err(new_error(ErrorType::TypeMismatch, format!("{self} is not indexable")))
			}
		}
	}
}

impl Display for CompositeType {
	fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
		match self {
			CompositeType::Str(s) => write!(fmt, "{}", s),
			CompositeType::Slice {parent, start_off, end_off, reverse} => {
				let parent = unsafe{ parent.as_ref().unwrap() };
				match parent {
					CompositeType::Str(string) => {
						if *reverse {
							let iter = string[*start_off..*end_off].chars().rev();
							for ch in iter {
								write!(fmt, "{ch}")?;
							}
						} else {
							let iter = string[*start_off..*end_off].chars();
							for ch in iter {
								write!(fmt, "{ch}")?;
							} 
						}
						Ok(())
					},
					// TODO: Add List slices
					CompositeType::Slice {..} | CompositeType::FRef {..} => panic!("Slices cannot have any parents other than Str or List.")
				}
			}
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

/*macro_rules! impl_rel_op {
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
}*/

trait FallibleComparison {
	fn try_compare(&self, other: &Self) -> Result<Ordering, FatalErr>;
}

impl FallibleComparison for BaseType {
	fn try_compare(&self, rv2: &BaseType) -> Result<Ordering, FatalErr> {
		match (self, rv2) {
			(BaseType::Int(i1), BaseType::Int(i2)) => Ok(i1.cmp(i2)),
			(BaseType::Flt(f1), BaseType::Flt(f2)) => f1.partial_cmp(f2).ok_or(new_error(ErrorType::IllegalValue, format!("Cannot compare {f1}, {f2}"))),
			(BaseType::Flt(f1), BaseType::Int(i2)) => f1.partial_cmp(&(*i2 as f64)).ok_or(new_error(ErrorType::IllegalValue, format!("Cannot compare {f1}, {i2}"))),
			(BaseType::Int(i1), BaseType::Flt(f2)) => (*i1 as f64).partial_cmp(f2).ok_or(new_error(ErrorType::IllegalValue, format!("Cannot compare {i1}, {f2}"))),
			(BaseType::Chr(c1), BaseType::Chr(c2)) => Ok(c1.cmp(c2)),
			(BaseType::Alloc(a1), BaseType::Alloc(a2)) => {a1.cmp(a2)},
			(BaseType::ConstRef(a1), BaseType::ConstRef(a2)) => unsafe {
				let a1 = a1.as_ref().unwrap();
				let a2 = a2.as_ref().unwrap();
				a1.cmp(a2)
			},
			_ => Err(new_error(ErrorType::TypeMismatch, format!("Unsupported comparison between {self}, {rv2}")))
		}
	}
}

impl FallibleComparison for char {
	fn try_compare(&self, other: &char) -> Result<Ordering, FatalErr> {
		Ok(self.cmp(other))
	}
}

impl_arith_op!(try_add, add, '+');
impl_arith_op!(try_mul, mul, '*');
impl_arith_op!(try_sub, sub, '-');
impl_arith_op!(try_rem, rem, '%');

macro_rules! impl_rel_op {
	($fname: ident, $func: ident) => {
		pub fn $fname (rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
			let ord =rv1.try_compare(rv2)?;
			Ok(BaseType::Int(ord.$func() as i64))
		}
	}
}

impl_rel_op!(try_lt, is_lt);
impl_rel_op!(try_gt, is_gt);
impl_rel_op!(try_eq, is_eq);
impl_rel_op!(try_lte,is_le);
impl_rel_op!(try_gte,is_ge);

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
pub fn as_printrepr(rv: &BaseType) -> Result<String,FatalErr> {
	Ok(match rv {
		BaseType::ConstRef(ptr) => {
			let ctype = unsafe { ptr.as_ref().ok_or(new_error(ErrorType::NullPointer, format!("{rv} is NULL and cannot be referenced.")))}?;
			format!("{ctype}")
		},
		BaseType::Alloc(a) => {
			let ctype = a.as_ref();
			format!("{ctype}")
		}
		_ => format!("{rv}")
	})
}

pub fn try_index(indexable: &BaseType, index_value: &BaseType) -> Result<BaseType, FatalErr> {
	match indexable {
		BaseType::ConstRef(ptr) => {
			let ctype = unsafe { ptr.as_ref().ok_or(new_error(ErrorType::NullPointer, format!("{indexable} is NULL and cannot be referenced.")))}?;
			ctype.getindex(index_value)
		},
		BaseType::MutRef(_ptr) => {
			todo!("Add mutable indexing for List")
		}
		_ => Err(new_error(ErrorType::TypeMismatch, format!("{indexable} cannot be indexed")))
	}
}

fn _lexicographical_ordering<T: FallibleComparison, I, J>(iter1: I, iter2: J) -> Result<Option<Ordering>, FatalErr>
	where
		I: Iterator<Item = T>,
		J: Iterator<Item = T>
	 {
	let dual = zip(iter1, iter2);
	for (v1, v2) in dual {
		let cmp = v1.try_compare(&v2)?;
		if let Ordering::Equal = cmp {
			continue;
		} else {
			return Ok(Some(cmp));
		}
	}
	Ok(None)
}

fn order_with_rev<T: FallibleComparison, I: DoubleEndedIterator<Item = T>>(iter1: I, iter2: I, rev1: bool, rev2: bool) -> Result<Option<Ordering>, FatalErr> {
	match (rev1, rev2) {
		(true, true) => {
			let iter1 = iter1.rev();
			let iter2 = iter2.rev();
			_lexicographical_ordering(iter1, iter2)
		},
		(true, false) => {
			let iter1 = iter1.rev();
			_lexicographical_ordering(iter1, iter2)	
		},
		(false, true) => {
			let iter2 = iter2.rev();
			_lexicographical_ordering(iter1, iter2)
		},
		(false, false) => {
			_lexicographical_ordering(iter1, iter2)
		}
	}
}