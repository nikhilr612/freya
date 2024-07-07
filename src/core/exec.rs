use std::io::BufRead;
use std::str::FromStr;
use core::time::Duration;
use std::sync::{Arc, RwLock};
use std::collections::{HashMap, hash_map::Entry};
use core::concat;

use crate::core::module::{ModulePool, ResolutionStatus, PoolWriteGuard};
use crate::core::types::{self, BaseType, FatalErr, ErrorType, new_error};
use crate::core::{op, FResult};
use crate::args::RefPolicy;
use crate::native::InterfacePool;

#[derive(Debug)]
struct CallFrame {
	/// Id of the module containing currently executing section.
	module_id: usize,
	/// Id of the function for debugging purposes.
	function_id: usize,
	/// Line number updated whenever LINENUMBER instruction is encountered.
	debug_lnum: u32,
	/// Vector of base types; indices simulate registers.
	regs: Vec<Option<BaseType>>,
	/// The register to write the return value into.
	rslot: Option<u8>,
	/// Address of instruction in the code region of module.
	ip: usize
}

impl CallFrame {
	fn from_fdecl(mp: &ModulePool, mpath: &str, ep: &str) -> (CallFrame, usize) {
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

	fn from_fnid(mp: &ModulePool, mid: usize, fid: usize) -> (CallFrame, usize) {
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
	fn get_reg_id(&self, r: u8) -> FResult<usize> {
		let r = r as usize;
		if r >= self.regs.len() {
			return Err(new_error(ErrorType::InvalidRegister, format!("Invalid register id {r}, max allocated {}", self.regs.len())));
		}
		Ok(r)
	}

	/// 'Read' register `r`.
	/// Returns an immutable reference to the value stored at index `r.
	/// Returns `Err` if register id `r` is invalid, or register is empty.
	fn read_register(&self, r: u8) -> FResult<&BaseType> {
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
	fn read_mutregst(&mut self, r: u8) -> FResult<&mut BaseType> {
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

	fn _write_unchecked(&mut self, rid: usize, val: Option<BaseType>, rc: &mut RefCounter) -> FResult<()>{
		if let Some(btype) = self.regs[rid].take() {
			Self::cleanup_value(btype, rc)?;
		}
		self.regs[rid] = val;
		Ok(())
	}

	/// 'Write' value `val` into register `r`, dropping ('cleaning up') the previous value.
	/// Performs any necessary reference decrements required.
	fn write_register(&mut self, r: u8, val: BaseType, rc: &mut RefCounter) -> FResult<()> {
		self.drop_register(r, rc)?;
		self.regs[r as usize].replace(val);
		Ok(())
	}

	/// 'Drop' value in register `r`. Does nothing if the register was empty.
	/// Performs any necessary reference decrements required.
	/// Returns `Err` if register `r` is invalid.
	fn drop_register(&mut self, r: u8, rc: &mut RefCounter) -> FResult<()> {
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

	fn move_register(&mut self, r1: u8, r2: u8, rc: &mut RefCounter) -> FResult<()> {
		let r1 = self.get_reg_id(r1)?;
		let v1 = self._take_register(r1, rc)?;
		let r2 = self.get_reg_id(r2)?;
		self._write_unchecked(r2, v1, rc)
	}

	fn _take_register(&mut self, r1: usize, rc: &mut RefCounter) -> FResult<Option<BaseType>> {
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

	fn dealloc_ctype(ctype: types::CompositeType, refc: &mut RefCounter) -> FResult<()> {
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

	fn cleanup_value(btype: BaseType, rc: &mut RefCounter) -> FResult<()> {
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

	fn _take_unchecked(&mut self, r1: u8) -> FResult<Option<BaseType>> {
		let r1 = self.get_reg_id(r1)?;
		Ok(self.regs[r1].take())
	}

	fn release(mut self, rc: &mut RefCounter) -> FResult<()> {
		// Double-pass, drop all references first, ...
		for r in self.regs.iter_mut() {
			let val = match r {
				None => continue,
				Some(v) => v
			};
			match val {
				BaseType::ConstRef(ptr) => { rc.decref(*ptr)?; },
				BaseType::MutRef(ptr) => {rc.decref_mut(*ptr)?;},
				// Anything else, drop in second pass
				_ => {continue;}
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
}

pub(crate) struct RefCounter {
	refctr: HashMap<usize, (usize, usize)>,
	refp: RefPolicy
}

impl RefCounter {
	fn new(rp: RefPolicy) -> RefCounter {
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
				let err = types::new_error(ErrorType::CoincidentRef, 
					format!("@{addr:x}:{r}, has an active mutable reference, and hence cannot acquire an immutable reference."));
				return self.error_or_warn(err);
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
				let err = types::new_error(ErrorType::CoincidentRef, format!("@{addr:x}:{r}, has {} active immutable reference(s), and hence cannot acquire mutable reference.", tup.0));
				return self.error_or_warn(err);
			}
			if tup.1 > 0 {
				return self.error_or_warn(types::new_error(ErrorType::MutableAliasing, format!("@{addr:x}:{r} already has an active mutable reference.")));
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
				return self.error_or_warn(types::new_error(ErrorType::NoRefsToDec, format!("@{addr:x} has no counted immutable references.")))
			}
		}
		self.error_or_warn(types::new_error(ErrorType::NoRefsToDec, format!("@{addr:x} has no counted immutable references.")))
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
	fn count_of(&self, addr: *const types::CompositeType) -> (usize, usize) {
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
			return self.error_or_warn(types::new_error(ErrorType::CoincidentRef, format!("@{addr:x}:{value} has an active mutable reference, hence cannot acquire immutable reference.")));
		}
		Ok(())
	}

	/// Verify that the value can be borrowed mutably.
	pub(crate) fn verify_borrow_mut(&self, value: &mut types::CompositeType) -> FResult<()> {
		let (icount, mcount) = self.count_of(value);
		let addr = (value as *const types::CompositeType) as usize;
		if icount > 0 {
			return self.error_or_warn(types::new_error(ErrorType::CoincidentRef, format!("@{addr:x}:{value} has active immutable reference(s), hence cannot acquire mutable reference.")));
		} else if mcount > 0 {
			return self.error_or_warn(types::new_error(ErrorType::MutableAliasing, format!("@{addr:x}:{value} already has an active mutable reference")));
		}
		Ok(())
	}
}

pub struct WorkerThread {
	/// Reference counter to keep track of number of active references during execution.
	refc: RefCounter,
	/// Stack to store current execution state and function information.
	stack: Vec<CallFrame>,
	/// Pool to load all modules required at runtime.
	/// Provides read-only access to modules.
	/// Pool is locked when a module is being loaded.
	pool: Arc<ModulePool>,
	nifp: Arc<RwLock<InterfacePool>>
}

fn resolve_extern(pool: &ModulePool, mut mvec: PoolWriteGuard, ext_ids: (usize, usize)) -> FResult<(usize, usize, bool)> {
	let ext = mvec[ext_ids.0].extern_ref(ext_ids.1);
	match ext.status {
		ResolutionStatus::Resolved => {							// We've already done this.
			Ok((ext.module_id, ext.function_id, ext.native))	// A-OK.
		},
		ResolutionStatus::Unresolved if !ext.native => {		// Unresolved non-native extern.
			let (module_id, function_id) = match pool.id_by_name(&ext.module_path) {
				Ok(module_id) => {	// if module is loaded retrieve function id
					let function_id = mvec[module_id].func_id(&ext.func_name)?;
					(module_id, function_id)
				},
				Err(_) => {			// otherwise load it.
					let pbuf = pool.resolve_path(&ext.module_path)?;
					let module = crate::module::open(pbuf.to_str().unwrap())?;
					let function_id = module.func_id(&ext.func_name)?;
					let module_id = mvec.len();
			 		pool.update_path(&ext.module_path, module_id);
					mvec.push(module);
					(module_id, function_id)
		 		}
			};
			let ext = mvec[ext_ids.0].extern_mutref(ext_ids.1);
			// Update extern, and change to resolved.
			ext.module_id = module_id;
			ext.function_id = function_id;
			ext.status = ResolutionStatus::Resolved;
			Ok((module_id, function_id, false))
		},
		ResolutionStatus::Unresolved => {
			todo!("Native functions have not been implemented yet.");
		}
	}
}

#[inline]
/// A call is deemed a 'tail-call' if:
/// -    it is immediately followed by a `VRET` (in all cases)
/// -    it is immediately followed by a `return <current return register>` (if call doesn't discard return value)
fn _check_tailcall(slvw: &mut crate::utils::SliceView, rslot: Option<u8>) -> bool {
	let nextinstr = slvw.get_u8();
	if nextinstr == op::VRET {
		return true;
	}
	if nextinstr == op::RET {
		let reg_id = slvw.get_u8();
		if let Some(c_reg_id) = rslot {
			return reg_id == c_reg_id
		}
		
	}
	false
}

// RN this method literally checks if value is FRef and copies its contents
fn _extract_finfo(val: &BaseType) -> FResult<(usize, usize, bool)>{
	if let BaseType::Alloc(cpt) = val {
		if let types::CompositeType::FRef { mod_id, func_idx, native } = &**cpt {
			Ok((*mod_id, *func_idx, *native))
		} else {
			Err(new_error(ErrorType::NotCallable, format!("Incompatible value. {val:?} is not a callable.")))
		}
	} else {
		Err(new_error(ErrorType::NotCallable, format!("Incompatible value. {val:?} is not a callable.")))
	}
}

fn _make_frame(param: Vec<Option<BaseType>>, pool: &ModulePool, mid: usize, fid: usize) -> FResult<CallFrame> {
	let (mut new_frame, narg) = CallFrame::from_fnid(pool, mid, fid);
	if param.len() != narg {
		let mvec = pool.read_lock();
		let name = mvec[mid].name_by_id(fid).unwrap();
		return Err(new_error(ErrorType::InvalidCall, format!("Function {name} requires {narg} parameters, but {} arguments were supplied.", param.len())));
	}
	// Put args.
	for (i, pval) in param.into_iter().enumerate() {
		new_frame.regs[i] = pval;
	}
	Ok(new_frame)
}

#[inline]
fn _get_line(prompt: &str) -> String {
	print!("{prompt}");
	let stdin = std::io::stdin();
	stdin.lock().lines().next().unwrap().unwrap()
}

macro_rules! instr_3arg {
	($func: expr, $slvw: ident, $frame: ident, $refc: expr) => {
		{
			let param = op::TripleRegst::try_from(&mut $slvw)?;
			
			let rv1 = $frame.read_register(param.r1)?;
			let rv2 = $frame.read_register(param.r2)?;
			let rid = param.r3;
			
			let res = $func(rv1, rv2)?;
			$frame.write_register(rid, res, &mut $refc)?;
		}
	};
}

macro_rules! instr_2arg {
	($slvw: ident, $frame: ident, $refc: expr, |$val:ident| $body: block) => {
		{
			let param = op::DoubleRegst::try_from(&mut $slvw)?;
			let $val = $frame.read_register(param.r1)?;
			let res = $body;
			$frame.write_register(param.r2, res, &mut $refc)?;
		}
	}
}

impl WorkerThread {
	/// Create a new WorkerThread for module `mpath` with entry point `ep`.
	#[allow(dead_code)]
	pub fn new(mpath: &str, ep: &str, refp: RefPolicy, mp: Arc<ModulePool>, nifp: Arc<RwLock<InterfacePool>>) -> WorkerThread {
		let (cf, _) = CallFrame::from_fdecl(&mp, mpath, ep);
		let stack = vec![cf]; // Load main onto stack.
		WorkerThread {
			refc: RefCounter::new(refp),
			stack,
			pool: mp,
			nifp
		}
	}

	/// Call a resolved callable in register `callr` with parameters `param` (possible in tail call manner if `is_tc`)
	fn _common_call(&mut self, callr: u8, param: Vec<Option<BaseType>>, is_tc: bool) -> FResult<()> {
		let val = self.stack.last().expect("Ultimate cockup. Common call initiated with empty stack").read_register(callr)?;
		let (mod_id, func_id, native) = _extract_finfo(val)?;

		if native {
			todo!("Implement native function calls.")
		}

		let new_frame = _make_frame(param, &self.pool, mod_id, func_id)?;

		if is_tc {
			let idx = self.stack.len() -1; 
			let old_frame = std::mem::replace(&mut self.stack[idx], new_frame);	// Replace the top-most frame in case of tail-call,
			old_frame.release(&mut self.refc)?									// and release the old frame
		} else {
			self.stack.push(new_frame);	    									// Push a new frame in the normal case.
		}

		Ok(())
	}

	/// Create a new WorkerThread for module `mpath` with entry point `ep`. 
	pub fn with_args<T>(mpath: &str, ep: &str, 
		refp: RefPolicy, mp: Arc<ModulePool>, nifp: Arc<RwLock<InterfacePool>>,
		args: T
	) -> WorkerThread
	where T: Iterator<Item = BaseType> {
		let (mut cf, n) = CallFrame::from_fdecl(&mp, mpath, ep);
		for (i, a) in args.take(n).enumerate() {
			cf.regs[i].replace(a);
		}
		let stack = vec![cf]; // Load main onto stack.
		WorkerThread {
			refc: RefCounter::new(refp),
			stack,
			pool: mp,
			nifp
		}
	}

	pub fn print_stack_trace(&self) {
		eprintln!("Trace back (most recent call last)");
		let mvec = self.pool.read_lock();
		for frame in &self.stack {
			let fmod = &mvec[frame.module_id];
			let fname = fmod.name_by_id(frame.function_id).unwrap();
			let mname = &fmod.name;
			if frame.debug_lnum != 0 {
				eprintln!("\t@{fname}\t[{}:{:#06x}], {mname}", frame.debug_lnum, frame.ip);
			} else {
				eprintln!("\t@{fname}\t[{:#06x}], {mname}", frame.ip)
			}
		}
	}

	/// Begin execution.
	pub fn begin(&mut self) -> FResult<()> {
		while !self.stack.is_empty() {
			let stack_top = self.stack.len() -1;
			let cur_frame = &mut self.stack[stack_top];					 // can be moved out
			let module_pool_vec_read = self.pool.read_lock();            // --> this HAS to stay here.
			let cur_module = &module_pool_vec_read[cur_frame.module_id]; // so this can't be outside loop either
			let mut slvw = cur_module.new_view(cur_frame.ip);			 // this also, ig ; but it doesn't really matter
			let opcode = slvw.get_u8();
			// Ugly match arm. Could do better.
			match opcode {
				op::VRET => { 
					self.stack.pop().expect("Cannot pop empty call stack.\n\tComment: Ultimate cockup.").release(&mut self.refc)?; // This should never panic.
					continue;
				},
				op::RET => {
					let r1 = slvw.get_u8();
					let mut frame = self.stack.pop().expect("Cannot pop empty call stack.\n\tComment: Ultimate cockup.");
					if !self.stack.is_empty() {	
						let cur_frame = &mut self.stack[stack_top-1];
						if let Some(ret_reg) = cur_frame.rslot {
							let val = frame._take_unchecked(r1)?;
							let ret_reg = ret_reg as usize;
							cur_frame._write_unchecked(ret_reg, val, &mut self.refc)?;
							cur_frame.rslot = None;
						}
					}
					frame.release(&mut self.refc)?;
					continue;
				},
				op::LDC => {
					let itype = op::Id16Reg::try_from(&mut slvw)?;
					let id = itype.id as usize;
					let mrfc = &mut self.refc;
					let val = cur_module.clone_constant(id, mrfc).unwrap();
					cur_frame.write_register(itype.r1, val, mrfc)?;
				},
				op::PRINT => {
					let rval = cur_frame.read_register(slvw.get_u8())?;
					println!("{}", types::as_printrepr(rval)?);
				},
				op::SLEEP => {
					let rv1 = cur_frame.read_register(slvw.get_u8())?;
					let dur = match rv1 {
						BaseType::Int(i) => Duration::from_secs(*i as u64),
						BaseType::Flt(f) => Duration::from_micros(f64::round(*f * 1e6) as u64),
						_ => {
							return Err(types::new_error(ErrorType::TypeMismatch, format!("Sleep requires NUMERIC type. Only Flt or Int. Cannot infer duration from {rv1}")));
						}
					};
					std::thread::sleep(dur);
				},
				op::ADD =>    instr_3arg!(types::try_add, slvw, cur_frame, self.refc),
				op::SUB =>    instr_3arg!(types::try_sub, slvw, cur_frame, self.refc),
				op::MUL =>    instr_3arg!(types::try_mul, slvw, cur_frame, self.refc),
				op::EXP =>    instr_3arg!(types::try_exp, slvw, cur_frame, self.refc),
				op::DIV =>    instr_3arg!(types::try_div, slvw, cur_frame, self.refc),
				op::IDIV =>   instr_3arg!(types::try_idiv, slvw, cur_frame, self.refc),
				op::REM =>    instr_3arg!(types::try_rem, slvw, cur_frame, self.refc),
				op::BAND =>   instr_3arg!(types::try_bitwise_and, slvw, cur_frame, self.refc),
				op::BOR =>    instr_3arg!(types::try_bitwise_or, slvw, cur_frame, self.refc),
				op::XOR =>    instr_3arg!(types::try_bitwise_xor, slvw, cur_frame, self.refc),
				op::LSHIFT => instr_3arg!(types::try_left_bitshift, slvw, cur_frame, self.refc),
				op::RSHIFT => instr_3arg!(types::try_right_bitshift, slvw, cur_frame, self.refc),
				op::RLT =>    instr_3arg!(types::try_lt, slvw, cur_frame, self.refc),
				op::RLE =>    instr_3arg!(types::try_lte, slvw, cur_frame, self.refc),
				op::RGT =>    instr_3arg!(types::try_gt, slvw, cur_frame, self.refc),
				op::RGE =>    instr_3arg!(types::try_gte, slvw, cur_frame, self.refc),
				op::REQ =>    instr_3arg!(types::try_eq, slvw, cur_frame, self.refc),
				op::RNEQ =>   instr_3arg!(types::try_neq, slvw, cur_frame, self.refc),
				op::LDI => {
					let val = slvw.get_u8() as i8;
					let out_reg = slvw.get_u8();
					let val = types::BaseType::Int(val as i64);
					cur_frame.write_register(out_reg, val, &mut self.refc)?;
				},
				op::SWAP => {
					let dreg = op::DoubleRegst::try_from(&mut slvw)?;
					let r1 = dreg.r1 as usize % cur_frame.regs.len();
					let r2 = dreg.r2 as usize % cur_frame.regs.len();
					cur_frame.regs.swap(r1, r2);
				},
				op::IINC => {
					let param = op::Id16Reg::try_from(&mut slvw)?;
					let rv1 = cur_frame.read_register(param.r1)?;
					let rv2 = types::BaseType::Int((param.id as i16) as i64);
					let res = types::try_add(rv1, &rv2)?;
					cur_frame.write_register(param.r1, res, &mut self.refc)?;
				},
				op::INC1 => {
					let r1 = slvw.get_u8();
					let rv1 = cur_frame.read_register(r1)?;
					let rv2 = types::BaseType::Int(1);
					let res = types::try_add(rv1, &rv2)?;
					cur_frame.write_register(r1, res, &mut self.refc)?;
				},
				op::MOV => {
					let dreg = op::DoubleRegst::try_from(&mut slvw)?;
					cur_frame.move_register(dreg.r1, dreg.r2, &mut self.refc)?;
				},
				op::DROP => {
					let r1 = slvw.get_u8();
					cur_frame.drop_register(r1, &mut self.refc)?;
				},
				op::LDF => {
					let param = op::Id16Reg::try_from(&mut slvw)?;
					let val = types::CompositeType::new_fref(cur_frame.module_id, param.id as usize, false);
					cur_frame.write_register(param.r1, val, &mut self.refc)?;
				},
				op::LDX => {
					let param = op::Id16Reg::try_from(&mut slvw)?;
					cur_frame.ip = slvw.offset();
					std::mem::drop(module_pool_vec_read);
					let val = {
						let mvec = self.pool.write_lock();
						resolve_extern(&self.pool, mvec, (cur_frame.module_id, param.id as usize))
					}?;
					cur_frame.write_register(param.r1, types::CompositeType::new_fref(val.0, val.1, val.2), &mut self.refc)?;
					continue;
				},
				op::LNOT => {
					let r1 = slvw.get_u8();
					let rv1 = cur_frame.read_register(r1)?;
					let res = types::try_lnot(rv1)?;
					cur_frame.write_register(r1, res, &mut self.refc)?;
				},
				op::JUMP => {
					let addr = slvw.get_u32();
					cur_frame.ip = addr as usize;
					continue;
				},
				op::BRANCH => {
					let param = op::RegAddr::try_from(&mut slvw)?;
					let rv = cur_frame.read_register(param.r1)?;
					if !types::as_bool(rv) {
						cur_frame.ip = param.addr as usize;
						continue;
					}
				}
				op::STDCALL => {
					// Read instruction					
					let dreg = op::DoubleRegst::try_from(&mut slvw)?;
					cur_frame.rslot = if dreg.r2 != 0 {Some(dreg.r2-1)} else {None};
					let rparam = op::VariadicRegst::try_from(&mut slvw)?;
					
					// Set ip to next instr, for execution after return
					cur_frame.ip = slvw.offset();
					let is_tc = _check_tailcall(&mut slvw, cur_frame.rslot); // Also check if TCO is possible.

					// Copy/Move arguments
					let mut param = Vec::<Option<BaseType>>::new();
					for reg in rparam.regs {
						let rid = cur_frame.get_reg_id(reg)?;
						let val = cur_frame._take_register(rid, &mut self.refc)?;
						param.push(val);
					}

					// Drop this guard here; along with cur_frame, etc.
					std::mem::drop(module_pool_vec_read);

					// Read FRef, they are already resolved now.
					self._common_call(dreg.r1, param, is_tc)?;

					continue;
				},
				op::FSTCALL => {
					let qreg = op::QuadrupleRegst::try_from(&mut slvw)?;
					cur_frame.rslot = if qreg.r2 != 0 {Some(qreg.r2-1)} else {None};

					// Set ip to next instr, for execution after return.
					cur_frame.ip = slvw.offset();
					let is_tc = _check_tailcall(&mut slvw, cur_frame.rslot); // Check if TCO is possible.
					
					// Prepare arguments.
					let param: FResult<Vec<_>> = (qreg.s1..(qreg.s1+qreg.s2)).map(|rid| {
						let rid = cur_frame.get_reg_id(rid)?;
						cur_frame._take_register(rid, &mut self.refc)
					}).collect();

					// Drop this guard here; along with cur_frame, etc.
					std::mem::drop(module_pool_vec_read);

					// Read FRef, they are already resolved now.
					self._common_call(qreg.r1, param?, is_tc)?;

					continue;
				},
				op::BWI => {
					let dreg = op::DoubleRegst::try_from(&mut slvw)?;
					let val = cur_frame.read_register(dreg.r1)?;
					let cref = match val {
						BaseType::Alloc(v) => v.as_ref() as *const types::CompositeType,
						_ => {
							return Err(new_error(ErrorType::TypeMismatch, format!("Cannot borrow {val}")));
						}
					};
					self.refc.incref(cref)?;
					cur_frame.write_register(dreg.r2, BaseType::ConstRef(cref), &mut self.refc)?;
				},
				op::BWM => {
					let dreg = op::DoubleRegst::try_from(&mut slvw)?;
					let val = cur_frame.read_mutregst(dreg.r1)?;
					let mref = match val {
						BaseType::Alloc(v) => v.as_mut() as *mut types::CompositeType,
						_ => {
							return Err(new_error(ErrorType::TypeMismatch, format!("Cannot borrow {val}")));
						}
					};
					self.refc.incref_mut(mref)?;
					cur_frame.write_register(dreg.r2, BaseType::MutRef(mref), &mut self.refc)?;
				},
				op::NOP => {},
				op::DBGLN => {
					let line_number = slvw.get_u32();
					cur_frame.debug_lnum = line_number;
				},
				op::ASSERT => {
					let dreg = op::DoubleRegst::try_from(&mut slvw)?;
					let val = cur_frame.read_register(dreg.r1)?;
					if !types::as_bool(val) {
						if usize::from(dreg.r2)>= cur_frame.regs.len() {
							return Err(new_error(ErrorType::AssertionFailure, format!("Value {val} evaluates to false")));
						} else {
							let msg = cur_frame.read_register(dreg.r2)?;
							return Err(new_error(ErrorType::AssertionFailure, format!("{msg}")));
						}
					}
				},
				op::PARSEINT => instr_2arg!(slvw, cur_frame, self.refc, |val| {
					let text = match types::as_composite_type(val, &self.refc)? { 
						types::CompositeType::Str(s) => s,
						_ => { return Err(new_error(ErrorType::TypeMismatch, format!("Cannot parse integer from {val} (not Str)")));}
					};
					BaseType::Int(i64::from_str(text).map_err(|_| {new_error(ErrorType::ParsingFailure, format!("Could not parse \"{text}\" as Int"))})?)
				}),
				op::PARSEFLT => instr_2arg!(slvw, cur_frame, self.refc, |val| {
					let text = match types::as_composite_type(val, &self.refc)? { 
						types::CompositeType::Str(s) => s,
						_ => { return Err(new_error(ErrorType::TypeMismatch, format!("Cannot parse float from {val} (not Str)"))); }
					};
					BaseType::Int(i64::from_str(text).map_err(|_| {new_error(ErrorType::ParsingFailure, format!("Could not parse \"{text}\" as Flt"))})?)
				}),
				op::GETLN => instr_2arg!(slvw, cur_frame, self.refc, |val| {BaseType::Alloc(Box::new(types::CompositeType::Str(_get_line(&types::as_printrepr(val)?))))}),
				op::LENGTH => instr_2arg!(slvw, cur_frame, self.refc, |val| {BaseType::Int((types::as_composite_type(val, &self.refc)?.length()?) as i64)}),
				op::BNOT => instr_2arg!(slvw, cur_frame, self.refc, |rval| { match rval {BaseType::Int(i1) => BaseType::Int(!*i1),_ => {return Err(types::new_error(ErrorType::TypeMismatch, format!("Unsupported bitwise not on {rval}")));}}}),
				op::FLOOR => instr_2arg!(slvw, cur_frame, self.refc, |rval| {types::try_floor(rval).ok_or(types::new_error(ErrorType::TypeMismatch, format!("Unsupported 'floor' on {rval}")))?}),
				op::ISN0 => instr_2arg!(slvw, cur_frame, self.refc, |rv| {if types::as_bool(rv) {types::BaseType::Int(1)} else {types::BaseType::Int(0)}}),
				op::FSLICE=>instr_2arg!(slvw, cur_frame, self.refc, |rv| {types::as_composite_type(rv, &self.refc)?.full_slice(&mut self.refc)?}),
				op::REVERSE => {
					let r1 = slvw.get_u8();
					let rval = cur_frame.read_mutregst(r1)?;
					types::as_composite_type_mut(rval, &self.refc)?.reverse_in_place()?
				}
				op::PUSH => {
					let dreg = op::DoubleRegst::try_from(&mut slvw)?;
					let r2 = cur_frame.get_reg_id(dreg.r2)?;
					let pval = cur_frame._take_register(r2, &mut self.refc)?.ok_or(new_error(ErrorType::EmptyRegister, format!("Cannot push value from empty register {r2}")))?;
					let rval = cur_frame.read_mutregst(dreg.r1)?;
					types::as_composite_type_mut(rval, &self.refc)?.push(pval)?;
				},
				op::POP => {
					let dreg = op::DoubleRegst::try_from(&mut slvw)?;
					let rval = cur_frame.read_mutregst(dreg.r1)?;
					let wval = types::as_composite_type_mut(rval, &self.refc)?.pop(&mut self.refc)?;
					cur_frame.write_register(dreg.r2, wval, &mut self.refc)?;
				},
				op::POPOR => {
					let breg = op::BiRegAddr::try_from(&mut slvw)?;
					let rval = types::as_composite_type_mut(cur_frame.read_mutregst(breg.r1)?, &self.refc)?;
					if let Ok(v) = rval.pop(&mut self.refc) {
						cur_frame.write_register(breg.r2, v, &mut self.refc)?;
					} else {
						cur_frame.ip = breg.addr as usize;
					}
				},
				op::SLICE => {
					let qreg = op::QuadrupleRegst::try_from(&mut slvw)?;
					let ctype = types::as_composite_type(cur_frame.read_register(qreg.r2)?, &self.refc)?;
					let s1 = cur_frame.read_register(qreg.s1)?;
					let s2 = cur_frame.read_register(qreg.s2)?;
					let val = ctype.slice(s1, s2, &mut self.refc)?;
					cur_frame.write_register(qreg.r1, val, &mut self.refc)?;
				},
				op::GETINDEX => {
					let treg = op::TripleRegst::try_from(&mut slvw)?;
					let rv1  = cur_frame.read_register(treg.r1)?;
					let rvi  = cur_frame.read_register(treg.r2)?;
					let val  = types::try_index(rv1, rvi, &mut self.refc)?;
					cur_frame.write_register(treg.r3, val, &mut self.refc)?;
				},
				op::PUTINDEX => {
					let treg = op::TripleRegst::try_from(&mut slvw)?;
					let r3 = cur_frame.get_reg_id(treg.r3)?;
					let r2 = cur_frame.get_reg_id(treg.r2)?;
					let rval = cur_frame._take_register(r3, &mut self.refc)?.ok_or(new_error(ErrorType::EmptyRegister, format!("Register {r3} is empty")))?;
					let idxv = cur_frame._take_register(r2, &mut self.refc)?.ok_or(new_error(ErrorType::EmptyRegister, format!("Register {r3} is empty")))?;
					let ctyp = cur_frame.read_mutregst(treg.r1)?;
					cur_frame.regs[r3] = types::try_putindex(ctyp, &idxv, rval, &self.refc)?;	// regs[r3] is None, so no need to write, or drop check
					cur_frame.regs[r2] = Some(idxv);							  				// Move the index back.
				},
				op::NEWLIST => {
					let reg = slvw.get_u8();
					let val = BaseType::Alloc(Box::new(types::CompositeType::List(Vec::new())));
					cur_frame.write_register(reg, val, &mut self.refc)?;
				},
				op::NEWBITS => {
					let dreg = op::DoubleRegst::try_from(&mut slvw)?;
					let rv1  = cur_frame.read_register(dreg.r1)?;
					let size = match rv1 {
						BaseType::Int(i) => {
							if *i < 0 {
								return Err(new_error(ErrorType::IllegalValue, format!("Value {i} is not a valid length.")));
							} else {
								*i as usize
							}
						},
						i => {
							return Err(new_error(ErrorType::IllegalValue, format!("Value {i} is not a valid length.")));
						}
					};
					let val = BaseType::Alloc(Box::new(types::CompositeType::BitSet(crate::utils::BitSet::new(size))));
					cur_frame.write_register(dreg.r2, val, &mut self.refc)?;
				},
				op::NEWRANGE => {
					let qreg = op::QuadrupleRegst::try_from(&mut slvw)?;
					let r2 = cur_frame.read_register(qreg.r2)?;
					let r3 = cur_frame.read_register(qreg.s1)?;
					let r4 = cur_frame.read_register(qreg.s2)?;
					let start = match r2 { BaseType::Int(i) => *i, _ => {
						return Err(new_error(ErrorType::TypeMismatch, format!("Cannot create range with {r2} as start")))	
					}};
					let end  = match r3 { BaseType::Int(i) => *i, _ => {
						return Err(new_error(ErrorType::TypeMismatch, format!("Cannot create range with {r3} as end")))	
					}};
					let step = match r4 { BaseType::Int(i) => *i, _ => {
						return Err(new_error(ErrorType::TypeMismatch, format!("Cannot create range with {r4} as end")))	
					}};
					if step == 0 {
						return Err(new_error(ErrorType::IllegalValue, "Cannot have zero step in range"))
					}
					let val = BaseType::Alloc(Box::new(types::CompositeType::Range {start, end, step}));
					cur_frame.write_register(qreg.r1, val, &mut self.refc)?;
				},
				op::NEWSTR => {
					let reg = slvw.get_u8();
					let val = BaseType::Alloc(Box::new(types::CompositeType::Str(String::new())));
					cur_frame.write_register(reg, val, &mut self.refc)?; 
				},
				op::ADVANCE => {
					let dreg = op::DoubleRegst::try_from(&mut slvw)?;
					let rv2  = cur_frame.read_register(dreg.r2)?;
					let newadv = match rv2 {
						BaseType::Int(i) => *i,
						a => {
							return Err(new_error(ErrorType::IllegalValue, format!("Value {a} is not a valid step.")));
						}
					};
					let rv1 = cur_frame.read_mutregst(dreg.r1)?;
					let ctype = types::as_composite_type_mut(rv1, &self.refc)?;
					match ctype {
						types::CompositeType::Slice { parent: _, start_off: _, end_off: _, advance_by, reverse} => {
							if newadv < 0 {
								*reverse = !(*reverse);
								*advance_by = (-newadv) as usize;
							} else {
								*advance_by = newadv as usize;
							}
						},
						a => {
							return Err(new_error(ErrorType::TypeMismatch, format!("Cannot set advance for {a}")));
						}
					}
				},
				_ => {
					return Err(new_error(ErrorType::InvalidOpcode, format!("Unrecognized opcode {opcode:x}")));
				}
			}
			cur_frame.ip =slvw.offset();
		}
		Ok(())
	}
}