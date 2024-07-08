//! Module to implement reference counting.

use std::collections::{HashMap, hash_map::Entry};
use crate::core::types::{self, FatalErr, ErrorType, new_error};
use crate::core::FResult;
use crate::args::RefPolicy;

/// Simple struct to store immutable and mutable reference counts of each `Alloc`-type.
pub struct RefCounter {
	refctr: HashMap<usize, (usize, usize)>,
	refp: RefPolicy
}

impl RefCounter {
	/// Create a new RefCounter with specified policy.
	pub(super) fn new(rp: RefPolicy) -> RefCounter {
		RefCounter {
			refctr: HashMap::new(),
			refp: rp
		}
	}

	#[inline(always)]
	fn error_or_warn(&self, err: FatalErr) -> FResult<()> {
		if let RefPolicy::WarnOnly = self.refp {
			eprintln!("WARNING: {}", err.message());
			Ok(())
		} else {
			Err(err)
		}
	}

	/// Increment the reference count of immutable references to an object.
	pub fn incref(&mut self, ptr: *const types::CompositeType) -> FResult<()> {
		if let RefPolicy::Inactive = self.refp {
			return Ok(())
		}

		let addr = ptr as usize;
		// Use Entry API to avoid double hash computation, at cost of pointer clone
		if let Entry::Vacant(e) = self.refctr.entry(addr) {
			e.insert((1, 0));
		} else {
			let tup = self.refctr.get_mut(&addr).unwrap();
			tup.0 += 1;
			if tup.1 != 0 {
				let r = unsafe {&*ptr};
				return self.error_or_warn(new_error(ErrorType::CoincidentRef, 
					format!("@{addr:x}:{r}, has an active mutable reference, and hence cannot acquire an immutable reference.")));
			}
		}
		Ok(())
	}

	/// Increment the reference count of mutable references to an object.
	pub fn incref_mut(&mut self, ptr: *mut types::CompositeType) -> FResult<()> {
		if let RefPolicy::Inactive = self.refp {
			return Ok(())
		}

		let addr = ptr as usize;
		if let Entry::Vacant(e) = self.refctr.entry(addr) {
			e.insert((0, 1));
		} else {
			let tup = self.refctr.get_mut(&addr).unwrap();
			let r = unsafe {&*ptr};
			if tup.0 > 0 {
				let errs = format!("@{addr:x}:{r}, has {} active immutable reference(s), and hence cannot acquire mutable reference.", tup.0);
				return self.error_or_warn(new_error(ErrorType::CoincidentRef,errs));
			}
			if tup.1 > 0 {
				return self.error_or_warn(
					new_error(ErrorType::MutableAliasing, 
						format!("@{addr:x}:{r} already has an active mutable reference.")));
			}
			tup.1 = 1;
		}
		Ok(())
	}

	/// Decrement the reference count of immutable references to an object.
	pub fn decref(&mut self, ptr: *const types::CompositeType) -> FResult<()> {
		if let RefPolicy::Inactive = self.refp {
			return Ok(())
		}

		let addr = ptr as usize;
		if self.refctr.contains_key(&addr) {
			let tup = self.refctr.get_mut(&addr).unwrap();
			if tup.0 > 0 {
				tup.0 -= 1;
				return Ok(());
			} else {
				return self.error_or_warn(
					new_error(ErrorType::NoRefsToDec, 
						format!("@{addr:x} has no counted immutable references.")))
			}
		}
		self.error_or_warn(
			new_error(ErrorType::NoRefsToDec,
				format!("@{addr:x} has no counted immutable references.")))
	}

	/// Decrement the reference count of mutable references to an object.
	pub fn decref_mut(&mut self, ptr: *mut types::CompositeType) -> FResult<()> {
		if let RefPolicy::Inactive = self.refp {
			return Ok(())
		}

		let addr = ptr as usize;
		if self.refctr.contains_key(&addr) {
			let tup = self.refctr.get_mut(&addr).unwrap();
			if tup.1 > 0 {
				tup.1 -= 1;
				return Ok(());
			} else {
				return self.error_or_warn(types::new_error(ErrorType::NoRefsToDec, format!("@{addr:x} has no counted mutable references.")));
			}
		}
		self.error_or_warn(types::new_error(ErrorType::NoRefsToDec, format!("@{addr:x} has no counted mutable references.")))
	}

	/// Get the number of active immutable and mutable references for a given composite type.
	pub(super) fn count_of(&self, addr: *const types::CompositeType) -> (usize, usize) {
		let addr = addr as usize;
		if self.refctr.contains_key(&addr) {
			*self.refctr.get(&addr).unwrap()
		} else {
			(0,0)
		}
	}

	/// Verify that the value can be borrowed immutably.
	pub(crate) fn verify_borrow(&self, value: &types::CompositeType) -> FResult<()> {
		let (_, mcount) = self.count_of(value);
		let addr = (value as *const types::CompositeType) as usize;
		if mcount > 0 {
			return self.error_or_warn(
				new_error(ErrorType::CoincidentRef, 
					format!("@{addr:x}:{value} has an active mutable reference, hence cannot acquire immutable reference.")
				));
		}
		Ok(())
	}

	/// Verify that the value can be borrowed mutably.
	pub(crate) fn verify_borrow_mut(&self, value: &mut types::CompositeType) -> FResult<()> {
		let (icount, mcount) = self.count_of(value);
		let addr = (value as *const types::CompositeType) as usize;
		if icount > 0 {
			return self.error_or_warn(
				new_error(ErrorType::CoincidentRef,
					format!("@{addr:x}:{value} has active immutable reference(s), hence cannot acquire mutable reference.")
				));
		} else if mcount > 0 {
			return self.error_or_warn(
				new_error(ErrorType::MutableAliasing, 
					format!("@{addr:x}:{value} already has an active mutable reference")
				));
		}
		Ok(())
	}
}