//! Module to implement a call frame.
use crate::core::module::ModulePool;
use crate::core::types::{self, BaseType, ErrorType, new_error};
use crate::core::FResult;

use super::RefCounter;

#[derive(Debug)]
pub struct CallFrame {
	/// Id of the module containing currently executing section.
	pub(super) module_id: usize,
	/// Id of the function for debugging purposes.
	function_id: usize,
	/// Line number updated whenever LINENUMBER instruction is encountered.
	debug_lnum: u16,
	/// Vector of base types; indices simulate registers.
	pub(super) regs: Vec<Option<BaseType>>,
	/// The register to write the return value into.
	pub(super) rslot: Option<u8>,
	/// Address of instruction in the code region of module.
	pub(super) ip: usize
}

impl CallFrame {
	pub fn from_fdecl(mp: &ModulePool, mpath: &str, ep: &str) -> (CallFrame, usize) {
		let mid = mp.id_by_name(mpath).unwrap_or_else(|e| panic!("Could not create CallFrame\n\tcause: {e}"));
		let mvec = mp.read_lock();	
		let m = &mvec[mid];
		let fnid = m.func_id(ep).unwrap_or_else(|e| panic!("Could not create CallFrame\n\tcause: {e}"));
		let fdc = m.fdecl_by_id(fnid);
		let n = fdc.nparam.into();
		let mut rvec = Vec::new();
		let ip = fdc.offset as usize;
		for _i in 0..fdc.nregs {
			rvec.push(None);
		}
		(CallFrame {
			module_id: mid,
			regs: rvec,
			rslot: None,
			function_id: fnid,
			debug_lnum: 0,
			ip
		}, n)
	}

	pub fn from_fnid(mp: &ModulePool, mid: usize, fid: usize) -> (CallFrame, usize) {
		let mvec = mp.read_lock();
		let m = &mvec[mid];
		let fdc = m.fdecl_by_id(fid);
		let n = fdc.nparam as usize;
		let mut rvec = Vec::new();
		let ip = fdc.offset as usize;
		for _i in 0..fdc.nregs {
			rvec.push(None);
		}
		(CallFrame {
			module_id: mid,
			regs: rvec,
			rslot: None,
			function_id: fid,
			debug_lnum: 0,
			ip
		}, n)
	}

	#[deprecated(
		since = "0.2.6",
		note = "Usage of this function is generally bad design in 'look before you leap' context."
	)]
	#[allow(dead_code)]
	pub fn get_reg_id(&self, r: u8) -> FResult<usize> {
		let r = r as usize;
		if r >= self.regs.len() {
			return Err(new_error(ErrorType::InvalidRegister, format!("Invalid register id {r}, max allocated {}", self.regs.len())));
		}
		Ok(r)
	}

	/// 'Read' register `r`.
	/// Returns an immutable reference to the value stored at index `r.
	/// Returns `Err` if register id `r` is invalid, or register is empty.
	pub fn read_register(&self, r: u8) -> FResult<&BaseType> {
		self.regs.get(r as usize)
		.ok_or_else(|| {
			new_error(ErrorType::InvalidRegister, 
				format!("Invalid register id {r}, max allocated {}",self.regs.len()))
		})?
		.as_ref()
		.ok_or_else(|| {
			new_error(ErrorType::EmptyRegister,
				format!("Register {r} is empty; expecting value."))
		})
	}

	/// 'Read' register `r` mutably.
	/// Returns `Err` if register id `r` is invalid, or register is empty.
	pub fn read_mutregst(&mut self, r: u8) -> FResult<&mut BaseType> {
		let n = self.regs.len();
		self.regs.get_mut(r as usize)
		.ok_or_else(|| {
			new_error(ErrorType::InvalidRegister, 
				format!("Invalid register id {r}, max allocated {n}"))
		})?
		.as_mut()
		.ok_or_else(|| {
			new_error(ErrorType::EmptyRegister,
				format!("Register {r} is empty; expecting value."))	
		})
	}

	pub fn writeopt_register(&mut self, rid: u8, val: Option<BaseType>, rc: &mut RefCounter) -> FResult<()>{
		self.drop_register(rid, rc)?;
		self.regs[rid as usize] = val;
		Ok(())
	}

	/// 'Write' value `val` into register `r`, dropping ('cleaning up') the previous value.
	/// Performs any necessary reference decrements required.
	pub fn write_register(&mut self, r: u8, val: BaseType, rc: &mut RefCounter) -> FResult<()> {
		self.drop_register(r, rc)?;
		self.regs[r as usize].replace(val);
		Ok(())
	}

	/// 'Drop' value in register `r`. Does nothing if the register was empty.
	/// Performs any necessary reference decrements required.
	/// Returns `Err` if register `r` is invalid.
	pub fn drop_register(&mut self, r: u8, rc: &mut RefCounter) -> FResult<()> {
		let n = self.regs.len();
		let v  = self.regs.get_mut(r as usize)
			.ok_or_else(|| {
				new_error(ErrorType::InvalidRegister, 
					format!("Invalid register id {r}, max allocated {n}"))
			})?
			.take();

		if let Some(b) = v {
			Self::cleanup_value(b, rc)?;
		}

		Ok(())
	}

	/// 'Take' value from register `r1`, and write it into register `r2`.
	/// This is **not** the same as _move_.
	pub fn tmove_register(&mut self, r1: u8, r2: u8, rc: &mut RefCounter) -> FResult<()> {
		let v1 = self.take_register(r1, rc)?;
		self.drop_register(r2, rc)?;
		self.regs[r2 as usize] = v1;
		Ok(())
	}

	#[deprecated(since = "0.2.6", note = "Bad method. Secretly unchecked. Use `take_register`.")]
	pub fn _take_register(&mut self, r1: usize, rc: &mut RefCounter) -> FResult<Option<BaseType>> {
		let btp = &self.regs[r1];
		if btp.is_none() {
			return Ok(None);
		}
		let btp = btp.as_ref().unwrap();
		// TOOD: Remove duplicate base type copy code
		let ret = 
			match btp {
				BaseType::Int(v) => {BaseType::Int(*v)},
				BaseType::Flt(v) => {BaseType::Flt(*v)},
				BaseType::Chr(v) => {BaseType::Chr(*v)},
				BaseType::Alloc(a) => {
					match a.try_copy() {
						Some(a) => BaseType::Alloc(Box::new(a)),
						None => self.regs[r1].take().unwrap()
					}
				},
				BaseType::ConstRef(v) => {
					// ConstRefs are copy on move.
					rc.incref(*v)?;
					BaseType::ConstRef(*v)
				},
				_ => {
					// Move mutable refs, OpaqueHandles
					self.regs[r1].take().unwrap()
				}
			}
		;
		Ok(Some(ret))
	}

	/// 'Take' a value from specified register `r`.
	/// 'Take' differs significantly from traditional _move_ or _copy_ semantics, in that that it is a hybrid of both.
	/// _Taking a value_ will result in copying whenever possible, otherwise it will move.
	/// Importantly, if value is a primitive base type (`Flt`, `Int`, `Chr`) or `ConstRef` it will always be copied,
	/// and if the value is a `MutRef` is will always be moved.
	pub fn take_register(&mut self, r: u8, rc: &mut RefCounter) -> FResult<Option<BaseType>> {
		let n = self.regs.len();
		let b = self.regs.get(r as usize)
		.ok_or_else(|| {
			new_error(ErrorType::InvalidRegister, 
				format!("Invalid register id {r}, max allocated {n}"))
		})?
		.as_ref();
		Ok(match b {
			None => None,
			Some(BaseType::Int(v)) => Some(BaseType::Int(*v)),
			Some(BaseType::Flt(v)) => Some(BaseType::Flt(*v)),
			Some(BaseType::Chr(v)) => Some(BaseType::Chr(*v)),
			Some(BaseType::Alloc(a)) => {
				match a.try_copy() {
					Some(a) => Some(BaseType::Alloc(Box::new(a))),
					None => Some(self.regs[r as usize].take().unwrap())
				}
			},
			Some(BaseType::ConstRef(v)) => {
				// ConstRefs are copy on move.
				rc.incref(*v)?;
				Some(BaseType::ConstRef(*v))
			},
			_ => {
				// Move mutable refs, OpaqueHandles
				Some(self.regs[r as usize].take().unwrap())
			}
		})
	}

	/// De-allocate a composite type, dropping any associated values, and releasing any associated resources.
	/// May call `cleanup` on values in composite types, resulting in recursive de-allocs.
	pub fn dealloc_ctype(ctype: types::CompositeType, refc: &mut RefCounter) -> FResult<()> {
		match ctype {
			types::CompositeType::FRef { .. }  => {
				// No alloc here.
			},
			types::CompositeType::Str(_s) => {
				// No sub objects owned.
			},
			types::CompositeType::BitSet(_b) => {
				// No sub objects owned.
			},
			types::CompositeType::Slice { parent, ..} => {
				// Has an immutable reference to parent object. Drop that reference.
				refc.decref(parent)?;
			},
			types::CompositeType::List(v) => {
				// Cleanup all sub-objects / values
				for value in v {
					Self::cleanup_value(value, refc)?
					// value is dropped.
				}
			},
			types::CompositeType::Range {..} => {
				// No alloc here.
			}
			/*,_ => {
				unimplemented!()
			}*/
		}
		Ok(())
	}

	/// 'Cleanup' a value. Does nothing if value is a primitive, i.e, `Flt`, `Int`, `Chr`.
	/// Performs any necessary reference decrements if value is `ConstRef` or `MutRef`.
	/// If value is `Alloc`, deallocates the underlying composite type.
	pub fn cleanup_value(btype: BaseType, rc: &mut RefCounter) -> FResult<()> {
		match btype {
			BaseType::ConstRef(r) => rc.decref(r),
			BaseType::MutRef(r) => rc.decref_mut(r),
			BaseType::Alloc(bx) => {
				let c = rc.count_of(bx.as_ref());
				if c != (0,0) {
					 Err(types::new_error(ErrorType::PrematureDealloc, 
					 	format!("Cannot deallocate {bx} whilst active references remain ({} immutable, {} mutable)", c.0, c.1)))
				} else { Self::dealloc_ctype(*bx, rc) }
			}
			_ => {
				Ok(())
			}
		}
	}

	#[deprecated(since = "0.2.6", note = "Does not conform to 'take' semantics. Prefer raw regs[r1].take() than this method.")]
	pub fn _take_unchecked(&mut self, r1: u8) -> FResult<Option<BaseType>> {
		let r1 = self.get_reg_id(r1)?; // This is so stupid. It says unchecked, but actually checks the register..
		Ok(self.regs[r1].take())
	}

	/// Release this `CallFrame` by dropping all references first, and then deallocating all owned composite types.
	/// References are dropped first in order to ensure that there are no `PrematureDealloc`s.
	pub fn release(mut self, rc: &mut RefCounter) -> FResult<()> {
		// Double-pass, drop all references first, ...
		for r in self.regs.iter_mut() {
			match r {
				Some(BaseType::ConstRef(ptr)) => { rc.decref(*ptr)?; },
				Some(BaseType::MutRef(ptr)) => {rc.decref_mut(*ptr)?;},
				_ => { continue; }
			}
			// Since r had a reference, which has now been dropped, we should skip it in next loop.
			*r = None;
		}
		// ... then drop the remaining regsiters starting from the back.
		for val in self.regs.into_iter().rev().flatten() {
			Self::cleanup_value(val, rc)?;
		}
		Ok(())
	}

	/// Set the line number for the frame; useful for debugging purposes.
	/// All instructions betweeen two successive calls of this function will be regarded as having line `debug_lnum`,
	pub fn set_lineno(&mut self, debug_lnum: u16) {
		self.debug_lnum = debug_lnum;
	}

	/// Get line number debug-info for this frame, i.e, value passed to the last call of `set_lineno`.
	pub fn lineno(&self) -> u16 {
		self.debug_lnum
	}

	pub fn function_id(&self) -> usize {
		self.function_id
	}
}