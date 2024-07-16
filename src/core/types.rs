use crate::exec::RefCounter;
use crate::utils::BitSet;
use core::fmt::Display;
use core::fmt::Formatter;
use std::cmp::Ordering;
use std::iter::zip;
use std::ops::{Add, BitAnd, BitOr, BitXor, Mul, Rem, Shl, Shr, Sub};

#[derive(Debug, Clone)]
pub enum ErrorType {
    ModuleLoadFailure,
    AssertionFailure,
    InternalFailure,
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
    UnknownLibrary,
    NativeLoadFailure,
    NoSuchSymbol,
    // NativeError
}

#[derive(Debug, Clone)]
pub struct FatalErr {
    error_t: ErrorType,
    msg: String,
}

pub fn new_error(errt: ErrorType, msg: impl Into<String>) -> FatalErr {
    FatalErr {
        error_t: errt,
        msg: msg.into(),
    }
}

impl FatalErr {
    pub(crate) fn message(&self) -> &str {
        &self.msg
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
        advance_by: usize,
        reverse: bool,
    },
    List(Vec<BaseType>), // TODO: Implement list operations.
    BitSet(BitSet),
    Range {
        start: i64,
        end: i64,
        step: i64,
    },
    // Table(HashMap<String, BaseType>), // TODO: Sort out details of dict implementation.
    FRef {
        mod_id: usize,
        func_idx: usize,
        native: bool,
    },
}

impl CompositeType {
    pub fn new_fref(mod_id: usize, func_idx: usize, native: bool) -> BaseType {
        BaseType::Alloc(Box::new(CompositeType::FRef {
            mod_id,
            func_idx,
            native,
        }))
    }

    fn cmp(&self, other: &CompositeType) -> Result<Ordering, FatalErr> {
        match (self, other) {
            (CompositeType::Str(s1), CompositeType::Str(s2)) => Ok(s1.cmp(s2)),
            (CompositeType::List(l1), CompositeType::List(l2)) => {
                Ok(order_with_rev(l1.iter(), l2.iter(), false, false)?
                    .unwrap_or(l1.len().cmp(&l2.len())))
            }
            (
                CompositeType::Slice {
                    parent: p1,
                    start_off: s_off1,
                    end_off: e_off1,
                    reverse: rev1,
                    ..
                },
                CompositeType::Slice {
                    parent: p2,
                    start_off: s_off2,
                    end_off: e_off2,
                    reverse: rev2,
                    ..
                },
            ) => {
                let p1 = unsafe {
                    p1.as_ref().ok_or(new_error(
                        ErrorType::NullPointer,
                        "Slice is over a NULL parent.",
                    ))
                }?;
                let p2 = unsafe {
                    p2.as_ref().ok_or(new_error(
                        ErrorType::NullPointer,
                        "Slice is over a NULL parent.",
                    ))
                }?;
                match (p1, p2) {
                    (CompositeType::Str(s1), CompositeType::Str(s2)) => {
                        let slice1 = &s1[*s_off1..*e_off1];
                        let slice2 = &s2[*s_off2..*e_off2];
                        Ok(
                            order_with_rev(slice1.chars(), slice2.chars(), *rev1, *rev2)?
                                .unwrap_or(slice1.len().cmp(&slice2.len())),
                        )
                    }
                    _ => Err(new_error(
                        ErrorType::TypeMismatch,
                        format!("Unsupported comparison between {p1}, {p2}"),
                    )),
                }
            }
            _ => Err(new_error(
                ErrorType::TypeMismatch,
                format!("Cannot compare {self:?} and {other:?}"),
            )),
        }
    }

    pub fn try_copy(&self) -> Option<CompositeType> {
        match self {
            CompositeType::FRef {
                mod_id,
                func_idx,
                native,
            } => Some(CompositeType::FRef {
                mod_id: *mod_id,
                func_idx: *func_idx,
                native: *native,
            }),
            CompositeType::Range { start, end, step } => Some(CompositeType::Range {
                start: *start,
                end: *end,
                step: *step,
            }),
            _ => None,
        }
    }

    pub fn length(&self) -> Result<usize, FatalErr> {
        match self {
            CompositeType::Str(s) => Ok(s.len()),
            CompositeType::List(v) => Ok(v.len()),
            CompositeType::Slice {
                parent: _,
                start_off,
                end_off,
                ..
            } => Ok(end_off - start_off),
            CompositeType::BitSet(b) => Ok(b.capacity()),
            CompositeType::Range { start, end, step } => {
                Ok(((end - start) / step).try_into().unwrap_or(0))
            }
            CompositeType::FRef { .. } => Err(new_error(
                ErrorType::TypeMismatch,
                format!("Unsupported 'length' on {self:?}"),
            )),
        }
    }

    pub(crate) fn push(&mut self, val: BaseType) -> Result<(), FatalErr> {
        match self {
            CompositeType::Str(s) => {
                s.push_str(&as_printrepr(&val)?);
                Ok(())
            }
            CompositeType::List(v) => {
                v.push(val);
                Ok(())
            }
            CompositeType::FRef { .. }
            | CompositeType::Slice { .. }
            | CompositeType::BitSet { .. }
            | CompositeType::Range { .. } => Err(new_error(
                ErrorType::TypeMismatch,
                format!("Unsupported 'push' on {self:?}"),
            )),
        }
    }

    pub(crate) fn pop(&mut self, refc: &mut RefCounter) -> Result<BaseType, FatalErr> {
        match self {
            CompositeType::Str(s) => {
                let ch = s.pop();
                match ch {
                    None => Err(new_error(
                        ErrorType::IllegalState,
                        "Cannot 'pop' from empty string.",
                    )),
                    Some(ch) => Ok(BaseType::Chr(ch)),
                }
            }
            CompositeType::Slice {
                parent,
                start_off,
                end_off,
                reverse,
                advance_by,
            } => {
                let parent = unsafe {
                    parent.as_ref().ok_or(new_error(
                        ErrorType::NullPointer,
                        "Slice is over a NULL parent.",
                    ))?
                };
                match parent {
                    CompositeType::Str(string) => {
                        let stringref = &string[*start_off..*end_off];
                        if *reverse {
                            let mut chars_iter = stringref.chars();
                            let ch = chars_iter.next().ok_or(new_error(
                                ErrorType::IllegalState,
                                "Cannot 'pop' from empty slice.",
                            ))?;
                            *start_off += ch.len_utf8();
                            let res = Ok(BaseType::Chr(ch));
                            for _ in 0..(*advance_by - 1) {
                                let tmp = chars_iter.next();
                                let ch = match tmp {
                                    None => {
                                        break;
                                    }
                                    Some(ch) => ch,
                                };
                                *start_off += ch.len_utf8();
                            }
                            res
                        } else {
                            let (index, ch) =
                                stringref.char_indices().next_back().ok_or(new_error(
                                    ErrorType::IllegalState,
                                    "Cannot 'pop' from empty slice.",
                                ))?;
                            *end_off = index + *start_off;
                            Ok(BaseType::Chr(ch))
                        }
                    }
                    CompositeType::List(v) => {
                        if *reverse {
                            let res = _copy_or_borrow(
                                v.get(*start_off).ok_or(new_error(
                                    ErrorType::IllegalState,
                                    "Cannot 'pop' from empty slice.",
                                ))?,
                                refc,
                            );
                            *start_off += *advance_by;
                            res
                        } else {
                            *end_off -= *advance_by;
                            _copy_or_borrow(
                                v.get(*end_off).ok_or(new_error(
                                    ErrorType::IllegalState,
                                    "Cannot 'pop' from empty slice.",
                                ))?,
                                refc,
                            )
                        }
                    }
                    // TODO: Add List Slices.
                    CompositeType::Slice { .. }
                    | CompositeType::FRef { .. }
                    | CompositeType::BitSet { .. }
                    | CompositeType::Range { .. } => {
                        panic!("Slices cannot have any parents other than Str or List.")
                    }
                }
            }
            CompositeType::List(v) => {
                let elm = v.pop();
                match elm {
                    None => Err(new_error(
                        ErrorType::IllegalState,
                        "Cannot 'pop' from empty list.",
                    )),
                    Some(val) => Ok(val),
                }
            }
            CompositeType::Range { start, end, step } => {
                if (*end - *start) * (*step) < 0 {
                    return Err(new_error(
                        ErrorType::IllegalState,
                        "Cannot 'pop' from empty range.",
                    ));
                }
                let ret = BaseType::Int(*start);
                *start += *step;
                Ok(ret)
            }
            CompositeType::FRef { .. } | CompositeType::BitSet { .. } => Err(new_error(
                ErrorType::TypeMismatch,
                format!("Unsupported 'pop' on {self:?}"),
            )),
        }
    }

    fn _partial_verify_index(idx: &BaseType, len: usize) -> Result<usize, FatalErr> {
        let idx = match idx {
            BaseType::Int(i) => *i,
            a => {
                return Err(new_error(
                    ErrorType::TypeMismatch,
                    format!("Cannot index with value {a}, must be Int"),
                ));
            }
        };
        let idx = if idx < 0 { idx + len as i64 } else { idx };
        if idx < 0 {
            return Err(new_error(
                ErrorType::InvalidIndex,
                format!("{idx}  < -{len}. Index is atleast -length"),
            ));
        }
        Ok(idx as usize)
    }

    fn _verify_index(idx: &BaseType, len: usize) -> Result<usize, FatalErr> {
        let idx = Self::_partial_verify_index(idx, len)?;
        if idx >= len {
            return Err(new_error(
                ErrorType::InvalidIndex,
                format!("{idx} >= {len}. Index is strictly less than length."),
            ));
        };
        Ok(idx)
    }

    fn _new_slice(
        s_off: &BaseType,
        e_off: &BaseType,
        len: usize,
    ) -> Result<(usize, usize, bool), FatalErr> {
        let s_off = Self::_partial_verify_index(s_off, len)
            .unwrap_or(0)
            .min(len);
        let e_off = Self::_partial_verify_index(e_off, len)
            .unwrap_or(0)
            .min(len);
        if s_off > e_off {
            Ok((e_off, s_off, true))
        } else {
            Ok((s_off, e_off, false))
        }
    }

    pub(crate) fn slice(
        &self,
        s1: &BaseType,
        s2: &BaseType,
        refc: &mut RefCounter,
    ) -> Result<BaseType, FatalErr> {
        match self {
            CompositeType::Str(s) => {
                let (s_off, e_off, rev) = Self::_new_slice(s1, s2, s.len())?;
                let parent = self as *const CompositeType;
                let ctype = CompositeType::Slice {
                    parent,
                    start_off: s_off,
                    end_off: e_off,
                    reverse: rev,
                    advance_by: 1,
                };
                refc.incref(parent)?;
                Ok(BaseType::Alloc(Box::new(ctype)))
            }
            CompositeType::List(v) => {
                let (s_off, e_off, rev) = Self::_new_slice(s1, s2, v.len())?;
                let parent = self as *const CompositeType;
                let ctype = CompositeType::Slice {
                    parent,
                    start_off: s_off,
                    end_off: e_off,
                    reverse: rev,
                    advance_by: 1,
                };
                refc.incref(parent)?;
                Ok(BaseType::Alloc(Box::new(ctype)))
            }
            CompositeType::Slice {
                parent,
                start_off,
                end_off,
                reverse,
                advance_by,
            } => {
                let len = end_off - start_off;
                refc.incref(*parent)?;
                let (s_off, e_off, rev) = Self::_new_slice(s1, s2, len)?;
                let ctype = if !reverse {
                    let real_s_off = start_off + s_off;
                    let real_e_off = start_off + e_off;
                    CompositeType::Slice {
                        parent: *parent,
                        start_off: real_s_off,
                        end_off: real_e_off,
                        reverse: rev,
                        advance_by: *advance_by,
                    }
                } else {
                    let real_s_off = end_off - s_off;
                    let real_e_off = end_off - e_off;
                    CompositeType::Slice {
                        parent: *parent,
                        start_off: real_e_off,
                        end_off: real_s_off,
                        reverse: !rev,
                        advance_by: *advance_by,
                    }
                };
                Ok(BaseType::Alloc(Box::new(ctype)))
            }
            a => Err(new_error(
                ErrorType::TypeMismatch,
                format!("Cannot slice value {a}"),
            )),
        }
    }

    pub(crate) fn full_slice(&self, refc: &mut RefCounter) -> Result<BaseType, FatalErr> {
        match self {
            CompositeType::List(v) => {
                let len = v.len();
                let parent = self as *const CompositeType;
                let ctype = CompositeType::Slice {
                    parent,
                    start_off: 0,
                    end_off: len,
                    reverse: false,
                    advance_by: 1,
                };
                refc.incref(parent)?;
                Ok(BaseType::Alloc(Box::new(ctype)))
            }
            CompositeType::Str(s) => {
                let len = s.len();
                let parent = self as *const CompositeType;
                let ctype = CompositeType::Slice {
                    parent,
                    start_off: 0,
                    end_off: len,
                    reverse: false,
                    advance_by: 1,
                };
                refc.incref(parent)?;
                Ok(BaseType::Alloc(Box::new(ctype)))
            }
            CompositeType::Slice {
                parent,
                start_off,
                end_off,
                reverse,
                advance_by,
            } => {
                let ctype = CompositeType::Slice {
                    parent: *parent,
                    start_off: *start_off,
                    end_off: *end_off,
                    reverse: *reverse,
                    advance_by: *advance_by,
                };
                refc.incref(*parent)?;
                Ok(BaseType::Alloc(Box::new(ctype)))
            }
            a => Err(new_error(
                ErrorType::TypeMismatch,
                format!("Cannot slice value {a}"),
            )),
        }
    }

    pub(crate) fn reverse_in_place(&mut self) -> Result<(), FatalErr> {
        match self {
            CompositeType::List(v) => {
                v.reverse();
                Ok(())
            }
            CompositeType::Slice {
                parent: _,
                start_off: _,
                end_off: _,
                reverse,
                ..
            } => {
                *reverse = !(*reverse);
                Ok(())
            }
            a => Err(new_error(
                ErrorType::TypeMismatch,
                format!("Unsupported in-place reversal of {a}"),
            )),
        }
    }

    fn _get_index_unchecked(
        &self,
        index: usize,
        refc: &mut RefCounter,
    ) -> Result<BaseType, FatalErr> {
        match self {
            CompositeType::Str(s) => Ok(BaseType::Chr((s.as_bytes())[index] as char)),
            CompositeType::List(v) => _copy_or_borrow(&v[index], refc),
            _ => panic!("Non-indexable composite-type somehow became parent of slice"),
        }
    }

    fn getindex(&self, idx: &BaseType, refc: &mut RefCounter) -> Result<BaseType, FatalErr> {
        match self {
            CompositeType::Str(s) => {
                let index = Self::_verify_index(idx, s.len())?;
                Ok(BaseType::Chr((s.as_bytes())[index] as char))
            }
            CompositeType::Slice {
                parent,
                start_off,
                end_off,
                reverse,
                ..
            } => {
                let index = Self::_verify_index(idx, end_off - start_off)?;
                let parent = unsafe_deref(*parent, self)?;
                if *reverse {
                    parent._get_index_unchecked(end_off - index, refc)
                } else {
                    parent._get_index_unchecked(start_off + index, refc)
                }
            }
            CompositeType::BitSet(b) => {
                let index = Self::_verify_index(idx, b.capacity())?;
                Ok(BaseType::Int(b.get_unchecked(index) as i64))
            }
            CompositeType::List(v) => {
                let index = Self::_verify_index(idx, v.len())?;
                _copy_or_borrow(&v[index], refc)
            }
            _ => Err(new_error(
                ErrorType::TypeMismatch,
                format!("{self} is not indexable"),
            )),
        }
    }

    fn getindex_mut(
        &mut self,
        idx: &BaseType,
        refc: &mut RefCounter,
    ) -> Result<BaseType, FatalErr> {
        match self {
            CompositeType::List(v) => {
                let index = Self::_verify_index(idx, v.len())?;
                _copy_or_borrow_mut(&mut v[index], refc)
            }
            CompositeType::BitSet(b) => {
                let index = Self::_verify_index(idx, b.capacity())?;
                Ok(BaseType::Int(b.get_unchecked(index) as i64))
            }
            CompositeType::Str(s) => {
                // Identical to get_index, just for convenience.
                let index = Self::_verify_index(idx, s.len())?;
                Ok(BaseType::Chr((s.as_bytes())[index] as char))
            }
            _ => Err(new_error(
                ErrorType::TypeMismatch,
                format!("Cannot index {self} via mutable reference."),
            )),
        }
    }

    /// Set value at given index and return the previous value, if any, for cleanup
    fn putindex(&mut self, idx: &BaseType, value: BaseType) -> Result<Option<BaseType>, FatalErr> {
        match self {
			CompositeType::List(v) => {
				let index = Self::_verify_index(idx, v.len())?;
				let v = std::mem::replace(&mut v[index], value);
				Ok(Some(v))
			},
			CompositeType::BitSet(b) => {
				let index = Self::_verify_index(idx, b.capacity())?;
				b.set_unchecked(index, as_bool(&value));
				Ok(Some(value))
			},
			CompositeType::Str(_) => {
				Err(new_error(ErrorType::TypeMismatch, "Overwriting contents of UTF-8 byte string at arbitrary indices is generally unsafe."))
			},
			_ => {
				Err(new_error(ErrorType::TypeMismatch, format!("Cannot set value at index for {self}")))
			}
		}
    }
}

fn display_list_slice(fmt: &mut Formatter<'_>, slice: &[BaseType]) -> Result<(), std::fmt::Error> {
    write!(fmt, "[")?;
    let mut flag = false;
    for v in slice.iter() {
        if flag {
            write!(fmt, ",")?;
        } else {
            flag = true;
        }
        write!(fmt, "{v}")?;
    }
    write!(fmt, "]")
}

impl Display for CompositeType {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CompositeType::Str(s) => write!(fmt, "{}", s),
            CompositeType::Slice {
                parent,
                start_off,
                end_off,
                reverse,
                ..
            } => {
                let parent = unsafe { parent.as_ref().unwrap() };
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
                    }
                    CompositeType::List(vec) => display_list_slice(fmt, &vec[*start_off..*end_off]),
                    // TODO: Add List slices
                    a => panic!("Slices cannot have any parents other than Str or List. {a}"),
                }
            }
            CompositeType::List(vec) => display_list_slice(fmt, vec),
            CompositeType::BitSet(bset) => {
                write!(fmt, "{bset}")
            }
            a => write!(fmt, "{:?}", a),
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
    // Unsigned pointer-width integer, intended for native APIs to use to return opaque handles to objects.
    OpaqueHandle(usize),
}

impl PartialEq for BaseType {
    fn eq(&self, other: &BaseType) -> bool {
        match (self, other) {
            (BaseType::Int(i1), BaseType::Int(i2)) => i1 == i2,
            (BaseType::Int(i1), BaseType::Flt(f2)) => *i1 as f64 == *f2,
            (BaseType::Flt(f2), BaseType::Int(i1)) => *i1 as f64 == *f2,
            (BaseType::Flt(f1), BaseType::Flt(f2)) => f1 == f2,
            (BaseType::Chr(c1), BaseType::Chr(c2)) => c1 == c2,
            (BaseType::Alloc(a1), BaseType::Alloc(a2)) => {
                let cp = a1.cmp(a2);
                match cp {
                    Ok(o) => o.is_eq(),
                    Err(_) => false,
                }
            }
            (BaseType::ConstRef(c1), BaseType::ConstRef(c2)) => c1 == c2,
            (BaseType::MutRef(c1), BaseType::MutRef(c2)) => c1 == c2,
            //(BaseType::OpaqueHandle(h1), BaseType::OpaqueHandle(h2)) => h1 == h2,
            _ => false,
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
            BaseType::OpaqueHandle(h1) => write!(fmt, "<ptr@{:x}>", { *h1 }),
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
                Ok(BaseType::Int(
                    i64::checked_pow(*a, *b as u32).unwrap_or(i64::MAX),
                ))
            }
        }
        (BaseType::Flt(a), BaseType::Int(b)) => Ok(BaseType::Flt(f64::powf(*a, *b as f64))),
        (BaseType::Int(a), BaseType::Flt(b)) => Ok(BaseType::Flt(f64::powf(*a as f64, *b))),
        (BaseType::Flt(a), BaseType::Flt(b)) => Ok(BaseType::Flt(f64::powf(*a, *b))),
        _ => Err(new_error(
            ErrorType::TypeMismatch,
            format!("Could not compute {v1} raised to {v2}"),
        )),
    }
}

pub fn try_floor(v1: &BaseType) -> Option<BaseType> {
    match v1 {
        BaseType::Int(i1) => Some(BaseType::Int(*i1)),
        BaseType::Flt(f1) => Some(BaseType::Int(f64::floor(*f1) as i64)),
        _ => None,
    }
}

pub fn try_idiv(v1: &BaseType, v2: &BaseType) -> Result<BaseType, FatalErr> {
    let i1 = match v1 {
        BaseType::Int(i1) => *i1,
        BaseType::Flt(f1) => *f1 as i64,
        _ => {
            return Err(new_error(
                ErrorType::TypeMismatch,
                format!("Unsupported '//' between {v1}, {v2}"),
            ));
        }
    };
    let i2 = match v2 {
        BaseType::Int(i1) => *i1,
        BaseType::Flt(f1) => *f1 as i64,
        _ => {
            return Err(new_error(
                ErrorType::TypeMismatch,
                format!("Unsupported '//' between {v1}, {v2}"),
            ));
        }
    };
    if i2 == 0 {
        return Err(new_error(ErrorType::ZeroDivision, "Cannot divide by zero"));
    }
    Ok(BaseType::Int(i1 / i2))
}

pub fn try_div(v1: &BaseType, v2: &BaseType) -> Result<BaseType, FatalErr> {
    let i1 = match v1 {
        BaseType::Int(i1) => *i1 as f64,
        BaseType::Flt(f1) => *f1,
        _ => {
            return Err(new_error(
                ErrorType::TypeMismatch,
                format!("Unsupported '/' between {v1}, {v2}"),
            ));
        }
    };
    let i2 = match v2 {
        BaseType::Int(i1) => *i1 as f64,
        BaseType::Flt(f1) => *f1,
        _ => {
            return Err(new_error(
                ErrorType::TypeMismatch,
                format!("Unsupported '/' between {v1}, {v2}"),
            ));
        }
    };
    if i2 == 0.0 {
        return Err(new_error(ErrorType::ZeroDivision, "Cannot divide by zero"));
    }
    Ok(BaseType::Flt(i1 / i2))
}

macro_rules! impl_bit_op {
    ($defined_name: ident, $function: ident, $ch: literal) => {
        #[inline]
        pub fn $defined_name(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
            match (rv1, rv2) {
                (BaseType::Int(a), BaseType::Int(b)) => Ok(BaseType::Int(a.$function(b))),
                _ => {
                    return Err(new_error(
                        ErrorType::TypeMismatch,
                        format!("Unsupported {} between {rv1}, {rv2}", $ch),
                    ));
                }
            }
        }
    };
}

impl_bit_op!(try_bitwise_and, bitand, '&');
impl_bit_op!(try_bitwise_or, bitor, '|');
impl_bit_op!(try_left_bitshift, shl, "<<");
impl_bit_op!(try_right_bitshift, shr, ">>");
impl_bit_op!(try_bitwise_xor, bitxor, "<<");

/// Attempt to copy the contents of this value.
/// Fails if base type is MutRef or OpaqueHandle.
fn _copy_or_borrow(val: &BaseType, refc: &mut RefCounter) -> Result<BaseType, FatalErr> {
    match val {
        BaseType::Int(i1) => Ok(BaseType::Int(*i1)),
        BaseType::Flt(f1) => Ok(BaseType::Flt(*f1)),
        BaseType::Chr(ch) => Ok(BaseType::Chr(*ch)),
        BaseType::ConstRef(ptr) => {
            let ptr = *ptr;
            refc.incref(ptr)?;
            Ok(BaseType::ConstRef(ptr))
        }
        BaseType::Alloc(a) => {
            if let Some(v) = a.try_copy() {
                // Copy whenever you can
                return Ok(BaseType::Alloc(Box::new(v)));
            }
            // Otherwise, try borrow
            let aref = a.as_ref() as *const CompositeType;
            refc.incref(aref)?;
            Ok(BaseType::ConstRef(aref))
        }
        _ => Err(new_error(
            ErrorType::TypeMismatch,
            format!("{val} can neither be copied nor borrowed"),
        )),
    }
}

/// Attempt to copy the contents of this value.
/// Fails if base type is MutRef or OpaqueHandle.
fn _copy_or_borrow_mut(val: &mut BaseType, refc: &mut RefCounter) -> Result<BaseType, FatalErr> {
    match val {
        BaseType::Int(i1) => Ok(BaseType::Int(*i1)),
        BaseType::Flt(f1) => Ok(BaseType::Flt(*f1)),
        BaseType::Chr(ch) => Ok(BaseType::Chr(*ch)),
        BaseType::ConstRef(ptr) => {
            let ptr = *ptr;
            refc.incref(ptr)?;
            Ok(BaseType::ConstRef(ptr))
        }
        BaseType::Alloc(a) => {
            if let Some(v) = a.try_copy() {
                // Copy whenever you can
                return Ok(BaseType::Alloc(Box::new(v)));
            }
            // Or borrow mutably
            let aref = a.as_mut() as *mut CompositeType;
            refc.incref_mut(aref)?;
            Ok(BaseType::MutRef(aref))
        }
        _ => Err(new_error(
            ErrorType::TypeMismatch,
            format!("{val} can neither be copied nor borrowed"),
        )),
    }
}

trait FallibleComparison {
    fn try_compare(self, other: Self) -> Result<Ordering, FatalErr>;
}

impl FallibleComparison for &BaseType {
    fn try_compare(self, rv2: &BaseType) -> Result<Ordering, FatalErr> {
        match (self, rv2) {
            (BaseType::Int(i1), BaseType::Int(i2)) => Ok(i1.cmp(i2)),
            (BaseType::Flt(f1), BaseType::Flt(f2)) => f1.partial_cmp(f2).ok_or(new_error(
                ErrorType::IllegalValue,
                format!("Cannot compare {f1}, {f2}"),
            )),
            (BaseType::Flt(f1), BaseType::Int(i2)) => {
                f1.partial_cmp(&(*i2 as f64)).ok_or(new_error(
                    ErrorType::IllegalValue,
                    format!("Cannot compare {f1}, {i2}"),
                ))
            }
            (BaseType::Int(i1), BaseType::Flt(f2)) => {
                (*i1 as f64).partial_cmp(f2).ok_or(new_error(
                    ErrorType::IllegalValue,
                    format!("Cannot compare {i1}, {f2}"),
                ))
            }
            (BaseType::Chr(c1), BaseType::Chr(c2)) => Ok(c1.cmp(c2)),
            (BaseType::Alloc(a1), BaseType::Alloc(a2)) => a1.cmp(a2),
            (BaseType::ConstRef(a1), BaseType::ConstRef(a2)) => unsafe {
                let a1 = a1.as_ref().unwrap();
                let a2 = a2.as_ref().unwrap();
                a1.cmp(a2)
            },
            _ => Err(new_error(
                ErrorType::TypeMismatch,
                format!("Unsupported comparison between {self}, {rv2}"),
            )),
        }
    }
}

impl FallibleComparison for char {
    fn try_compare(self, other: char) -> Result<Ordering, FatalErr> {
        Ok(self.cmp(&other))
    }
}

impl_arith_op!(try_add, add, '+');
impl_arith_op!(try_mul, mul, '*');
impl_arith_op!(try_sub, sub, '-');
impl_arith_op!(try_rem, rem, '%');

macro_rules! impl_rel_op {
    ($fname: ident, $func: ident) => {
        pub fn $fname(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
            let ord = rv1.try_compare(rv2)?;
            Ok(BaseType::Int(ord.$func() as i64))
        }
    };
}

impl_rel_op!(try_lt, is_lt);
impl_rel_op!(try_gt, is_gt);
impl_rel_op!(try_lte, is_le);
impl_rel_op!(try_gte, is_ge);

pub fn try_eq(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
    Ok(BaseType::Int(rv1.eq(rv2) as i64))
}

#[inline]
pub fn try_neq(rv1: &BaseType, rv2: &BaseType) -> Result<BaseType, FatalErr> {
    match try_eq(rv1, rv2) {
        Ok(b) => {
            if let BaseType::Int(i1) = b {
                Ok(BaseType::Int(1 - i1))
            } else {
                Ok(BaseType::Int(0))
            }
        }
        _ => Ok(BaseType::Int(0)),
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
        }
        _ => Err(new_error(
            ErrorType::TypeMismatch,
            format!("Unsupported 'logical NOT' on {rv1}"),
        )),
    }
}

#[inline]
pub fn as_bool(rv: &BaseType) -> bool {
    match rv {
        BaseType::Int(i1) => *i1 != 0,
        BaseType::Flt(f1) => *f1 != 0.0,
        BaseType::Chr(c1) => *c1 != '\0',
        BaseType::Alloc(a) => {
            let a = a.as_ref();
            match a {
                CompositeType::Str(s) => !s.is_empty(),
                CompositeType::List(v) => !v.is_empty(),
                CompositeType::Slice {
                    parent: _,
                    start_off,
                    end_off,
                    ..
                } => start_off < end_off,
                _ => true,
            }
        }
        _ => true,
    }
}

pub(crate) fn as_composite_type<'a>(
    rv: &'a BaseType,
    refc: &RefCounter,
) -> Result<&'a CompositeType, FatalErr> {
    let ret = match rv {
        BaseType::Alloc(a) => a.as_ref(),
        BaseType::ConstRef(ptr) => unsafe_deref(*ptr, rv)?,
        _ => {
            return Err(new_error(
                ErrorType::TypeMismatch,
                format!("{rv} is NOT a composite type."),
            ))
        }
    };
    refc.verify_borrow(ret)?;
    Ok(ret)
}

pub(crate) fn as_composite_type_mut<'a>(
    rv: &'a mut BaseType,
    refc: &RefCounter,
) -> Result<&'a mut CompositeType, FatalErr> {
    match rv {
        BaseType::Alloc(a) => {
            let mref = a.as_mut();
            refc.verify_borrow_mut(mref)?;
            Ok(mref)
        }
        BaseType::MutRef(ptr) => Ok(unsafe_deref_mut(*ptr, rv)?),
        _ => Err(new_error(
            ErrorType::TypeMismatch,
            format!("{rv} is NOT a composite type."),
        )),
    }
}

#[inline]
pub fn as_printrepr(rv: &BaseType) -> Result<String, FatalErr> {
    Ok(match rv {
        BaseType::ConstRef(ptr) => {
            let ctype = unsafe_deref(*ptr, rv)?;
            format!("{ctype}")
        }
        BaseType::Alloc(a) => {
            let ctype = a.as_ref();
            format!("{ctype}")
        }
        _ => format!("{rv}"),
    })
}

#[inline(always)]
fn unsafe_deref<A, B: Display>(ptr: *const A, value: &B) -> Result<&A, FatalErr> {
    unsafe {
        ptr.as_ref().ok_or(new_error(
            ErrorType::NullPointer,
            format!("{value} is NULL and cannot be referenced."),
        ))
    }
}

#[inline(always)]
fn unsafe_deref_mut<A, B: Display>(ptr: *mut A, value: &B) -> Result<&mut A, FatalErr> {
    unsafe {
        ptr.as_mut().ok_or(new_error(
            ErrorType::NullPointer,
            format!("{value} is NULL and cannot be referenced."),
        ))
    }
}

pub(crate) fn try_index(
    indexable: &BaseType,
    index_value: &BaseType,
    refc: &mut RefCounter,
) -> Result<BaseType, FatalErr> {
    match indexable {
        BaseType::ConstRef(ptr) => {
            let ctype = unsafe_deref(*ptr, indexable)?;
            ctype.getindex(index_value, refc)
        }
        BaseType::MutRef(ptr) => {
            let ctype = unsafe_deref_mut(*ptr, indexable)?;
            ctype.getindex_mut(index_value, refc)
        }
        BaseType::Alloc(a) => {
            let ctype = a.as_ref();
            refc.verify_borrow(a)?;
            ctype.getindex(index_value, refc)
        }
        _ => Err(new_error(
            ErrorType::TypeMismatch,
            format!("{indexable} cannot be indexed"),
        )),
    }
}

pub(crate) fn try_putindex(
    indexable: &mut BaseType,
    index_value: &BaseType,
    value: BaseType,
    refc: &RefCounter,
) -> Result<Option<BaseType>, FatalErr> {
    match indexable {
        BaseType::MutRef(ptr) => {
            let ctype = unsafe_deref_mut(*ptr, indexable)?;
            ctype.putindex(index_value, value)
        }
        BaseType::Alloc(a) => {
            refc.verify_borrow_mut(a)?;
            let ctype = a.as_mut();
            ctype.putindex(index_value, value)
        }
        _ => Err(new_error(
            ErrorType::TypeMismatch,
            format!("Cannot set value at index for {indexable} without mutable reference"),
        )),
    }
}

fn _lexicographical_ordering<T: FallibleComparison, I, J>(
    iter1: I,
    iter2: J,
) -> Result<Option<Ordering>, FatalErr>
where
    I: Iterator<Item = T>,
    J: Iterator<Item = T>,
{
    let dual = zip(iter1, iter2);
    for (v1, v2) in dual {
        let cmp = v1.try_compare(v2)?;
        if let Ordering::Equal = cmp {
            continue;
        } else {
            return Ok(Some(cmp));
        }
    }
    Ok(None)
}

fn order_with_rev<T: FallibleComparison, I: DoubleEndedIterator<Item = T>>(
    iter1: I,
    iter2: I,
    rev1: bool,
    rev2: bool,
) -> Result<Option<Ordering>, FatalErr> {
    match (rev1, rev2) {
        (true, true) => {
            let iter1 = iter1.rev();
            let iter2 = iter2.rev();
            _lexicographical_ordering(iter1, iter2)
        }
        (true, false) => {
            let iter1 = iter1.rev();
            _lexicographical_ordering(iter1, iter2)
        }
        (false, true) => {
            let iter2 = iter2.rev();
            _lexicographical_ordering(iter1, iter2)
        }
        (false, false) => _lexicographical_ordering(iter1, iter2),
    }
}
