use std::io::BufRead;
use std::str::FromStr;
use core::time::Duration;
use std::sync::Arc;
use crate::module::ModulePool;
use crate::module::FResult;
use crate::module::ResolutionStatus;
use crate::types::BaseType;

use std::collections::HashMap;

use crate::types::new_error;
use crate::types::ErrorType;
use crate::op;
use crate::types;

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
	fn from_fdecl(mp: &ModulePool, mpath: &str, ep: &str) -> CallFrame {
		let mid = mp.id_by_name(mpath).map_err(|e| {eprintln!("Could not create CallFrame\n\tcause: {e}")}).unwrap();
		let mvec = mp.read_lock();	
		let m = &mvec[mid];
		let fnid = m.func_id(ep).map_err(|e| {eprintln!("Could not create CallFrame\n\tcause: {e}")}).unwrap();
		let fdc = m.fdecl_by_id(fnid);
		let mut rvec = Vec::new();
		let ip = fdc.offset as usize;
		for _i in 0..fdc.nregs {
			rvec.push(None);
		}
		CallFrame {
			module_id: mid,
			regs: rvec,
			rslot: None,
			function_id: fnid,
			debug_lnum: 0,
			ip: ip
		}
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
			ip: ip
		}, n)
	}

	fn get_reg_id(&self, r: u8) -> FResult<usize> {
		let r = r as usize;
		if r >= self.regs.len() {
			return Err(new_error(ErrorType::InvalidRegister, format!("Invalid register id {r}, max allocated {}", self.regs.len())));
		}
		Ok(r)
	}

	fn read_register<'a>(&self, r: u8) -> FResult<&BaseType> {
		let r = self.get_reg_id(r)?;
		let rval = match &self.regs[r] {
			Some(r) => r,
			None => {
				return Err(new_error(ErrorType::EmptyRegister, format!("Register {r} is empty; expecting value.")));
			}
		};
		Ok(rval)
	}

	fn read_mutregst<'a>(&mut self, r: u8) -> FResult<&mut BaseType> {
		let r = self.get_reg_id(r)?;
		let rval = match &mut self.regs[r] {
			Some(r) => r,
			None => {
				return Err(new_error(ErrorType::EmptyRegister, format!("Register {r} is empty; expecting value.")));
			}
		};
		Ok(rval)	
	}

	fn write_unchecked(&mut self, rid: usize, val: Option<BaseType>, rc: &mut RefCounter) -> FResult<()>{
		if let Some(btype) = self.regs[rid].take() {
			Self::cleanup_value(btype, rc)?;
		}
		self.regs[rid] = val;
		Ok(())
	}

	fn write_register(&mut self, r: u8, val: BaseType, rc: &mut RefCounter) -> FResult<()> {
		let rid = self.get_reg_id(r)?;
		self.write_unchecked(rid, Some(val), rc)
	}

	fn drop_register(&mut self, r: u8, rc: &mut RefCounter) -> FResult<()> {
		let rid = self.get_reg_id(r)?;
		self.write_unchecked(rid, None, rc)
	}

	fn move_register(&mut self, r1: u8, r2: u8, rc: &mut RefCounter) -> FResult<()> {
		// TODO: Re-implement using _take_register
		let r1 = self.get_reg_id(r1)?;
		let v1 = self._take_register(r1, rc)?;
		let r2 = self.get_reg_id(r2)?;
		self.write_unchecked(r2, v1, rc)
	}

	fn _take_register(&mut self, r1: usize, rc: &mut RefCounter) -> FResult<Option<BaseType>> {
		let btp = &self.regs[r1];
		if let None = btp {
			return Ok(None);
		}
		let btp = btp.as_ref().unwrap();
		let ret = 
			match btp {
				BaseType::Int(v) => {BaseType::Int(*v)},
				BaseType::Flt(v) => {BaseType::Flt(*v)},
				BaseType::Chr(v) => {BaseType::Chr(*v)},
				BaseType::Alloc(a) => {
					match a.as_ref() {
						// FRefs are immutable, no restrictions on copy.
						types::CompositeType::FRef { mod_id, func_idx, unext } => {
							let cloned =  types::CompositeType::FRef { mod_id: *mod_id, func_idx: *func_idx, unext: *unext };
							BaseType::Alloc(Box::new(cloned))
						},
						_ => self.regs[r1].take().unwrap()
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
			types::CompositeType::FRef { mod_id: _, func_idx: _, unext: _}  => {
				// No alloc here.
			},
			types::CompositeType::Str(_s) => {
				// No sub objects owned.
			},
			types::CompositeType::Slice { parent, ..} => {
				// Has an immutable reference to parent object. Drop that reference.
				refc.decref(parent)?;
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

	fn release(self, rc: &mut RefCounter) -> FResult<()> {
		for r in self.regs {
			if let Some(val) = r {
				Self::cleanup_value(val, rc)?;
			}
		}
		Ok(())
	}
}

#[derive(Debug)]
#[repr(u8)]
#[derive(Clone)]
enum RefPolicy {
	Inactive = 0,
	WarnOnly = 1,
	Strict	 = 2
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

	/// Increment the reference count of immutable references to an object.
	pub fn incref(&mut self, ptr: *const types::CompositeType) -> FResult<()> {
		if let RefPolicy::Inactive = self.refp {
			return Ok(())
		}
		let addr = ptr as usize;
		if self.refctr.contains_key(&addr) {
			let tup = self.refctr.get_mut(&addr).unwrap();
			tup.0 += 1;
			if tup.1 != 0 {
				let r = unsafe {&*ptr};
				return Err(types::new_error(ErrorType::CoincidentRef, format!("@{addr:x}:{r}, has an active mutable reference, and hence cannot acquire an immutable reference.")));
			}
		} else {
			self.refctr.insert(addr, (1, 0));
		}
		Ok(())
	}

	/// Increment the reference count of mutable references to an object.
	pub fn incref_mut(&mut self, ptr: *mut types::CompositeType) -> FResult<()> {
		if let RefPolicy::Inactive = self.refp {
			return Ok(())
		}
		let addr = ptr as usize;
		if self.refctr.contains_key(&addr) {
			let tup = self.refctr.get_mut(&addr).unwrap();
			let r = unsafe {&*ptr};
			if tup.0 > 0 {
				return Err(types::new_error(ErrorType::CoincidentRef, format!("@{addr:x}:{r}, has {} active immutable reference(s), and hence cannot acquire mutable reference.", tup.0)));
			}
			if tup.1 > 0 {
				return Err(types::new_error(ErrorType::MutableAliasing, format!("@{addr:x}:{r} already has an active mutable reference.")));
			}
			tup.1 = 1;
		} else {
			self.refctr.insert(addr, (0, 1));
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
				return Err(types::new_error(ErrorType::NoRefsToDec, format!("@{addr:x} has no counted immutable references.")))
			}
		}
		return Err(types::new_error(ErrorType::NoRefsToDec, format!("@{addr:x} has no counted immutable references.")));
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
				return Err(types::new_error(ErrorType::NoRefsToDec, format!("@{addr:x} has no counted mutable references.")));
			}
		}
		return Err(types::new_error(ErrorType::NoRefsToDec, format!("@{addr:x} has no counted mutable references.")));
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
			return Err(types::new_error(ErrorType::CoincidentRef, format!("@{addr:x}:{value} has an active mutable reference, hence cannot acquire immutable reference.")));
		}
		Ok(())
	}

	/// Verify that the value can be borrowed mutably.
	pub(crate) fn verify_borrow_mut(&self, value: &mut types::CompositeType) -> FResult<()> {
		let (icount, mcount) = self.count_of(value);
		let addr = (value as *const types::CompositeType) as usize;
		if icount > 0 {
			return Err(types::new_error(ErrorType::CoincidentRef, format!("@{addr:x}:{value} has active immutable reference(s), hence cannot acquire mutable reference.")));
		} else if mcount > 0 {
			return Err(types::new_error(ErrorType::MutableAliasing, format!("@{addr:x}:{value} already has an active mutable reference")));
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
}

#[inline]
fn _prep_fcall(pool: &ModulePool, mod_id: usize, func_idx: usize, unext: bool) -> FResult<(usize, usize)> {
	let mut mid = mod_id;
	let mut fid = func_idx;
	if unext {
		let mut mvec = pool.write_lock();

		let ext = &mvec[mod_id].extern_ref(func_idx);
		if pool.is_loaded(&ext.module_path) {
			mid = pool.id_by_name(&ext.module_path)?;
			fid = mvec[mid].func_id(&ext.func_name)?;
		} else {
			// Write, without acquiring another lock.
			let mut pbuf = std::env::current_dir().map_err(|e| {
 				let m = format!("Failed to create path to module {}, cause {e:?}", ext.module_path);
 				crate::types::new_error(ErrorType::ModuleLoadFailure, m)
 			})?;
 			let path = ext.module_path.clone();
 			pbuf.push(path.clone());
 			let module = crate::module::open(pbuf.to_str().unwrap())?;
 			fid = module.func_id(&ext.func_name)?;
 			mvec.push(module);
	 		mid = mvec.len()-1;
	 		pool.update_path(&path, mid);
 		}
 		let ext = &mut mvec[mod_id].extern_mutref(func_idx);
	 	ext.module_id  = mid;
	 	ext.function_id = fid;
	 	ext.status = ResolutionStatus::Resolved;

	 	std::mem::drop(mvec);
	}
	Ok((mid, fid))
}

#[inline]
/// A call is deemed a 'tail-call' if:
/// -	it is immediately followed by a `VRET` (in all cases)
/// -	it is immediately followed by a `return <current return register>` (if call doesn't discard return value)
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
	return false;
}

fn _extract_finfo(val: &BaseType) -> FResult<(usize, usize, bool)>{
	if let BaseType::Alloc(cpt) = val {
		if let types::CompositeType::FRef { mod_id, func_idx, unext} = &**cpt {
			return Ok((*mod_id, *func_idx, *unext));
		} else {
			return Err(new_error(ErrorType::NotCallable, format!("Incompatible value. {val:?} is not a callable.")));
		}
	} else {
		return Err(new_error(ErrorType::NotCallable, format!("Incompatible value. {val:?} is not a callable.")));
	}
}

fn _make_frame(mut param: Vec<Option<BaseType>>, pool: &ModulePool, mid: usize, fid: usize) -> FResult<CallFrame> {
	let (mut new_frame, narg) = CallFrame::from_fnid(pool, mid, fid);
	if param.len() != narg {
		let mvec = pool.read_lock();
		let name = mvec[mid].name_by_id(fid).unwrap();
		return Err(new_error(ErrorType::InvalidCall, format!("Function {name} requires {narg} parameters, but {} arguments were supplied.", param.len())));
	}
	// Put args.
	for i in 0..param.len() {
		new_frame.regs[i] = param[i].take();
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
	pub fn new<'a>(mpath: &str, ep: &str, refp: u8, mp: Arc<ModulePool>) -> WorkerThread {
		let cf = CallFrame::from_fdecl(&mp, mpath, ep);
		let mut stack = Vec::new();
		let refp = match refp {
			0 => RefPolicy::Inactive,
			1 => RefPolicy::WarnOnly,
			2 => RefPolicy::Strict,
			_ => RefPolicy::WarnOnly
		};
		stack.push(cf);	// Load main onto stack.
		WorkerThread {
			refc: RefCounter::new(refp),
			stack: stack,
			pool: mp
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
				eprintln!("\t@{fname}\t[{}:{:#06x}], {mname}", frame.ip, frame.debug_lnum);
			} else {
				eprintln!("\t@{fname}\t[{:#06x}], {mname}", frame.ip)
			}
		}
	}

	/// Begin execution.
	pub fn begin(&mut self) -> FResult<()> {
		while self.stack.len() > 0 {
			let stack_top = self.stack.len() -1;
			let cur_frame = &mut self.stack[stack_top];
			let module_pool_vec_read = self.pool.read_lock();
			let cur_module = &module_pool_vec_read[cur_frame.module_id];
			let mut slvw = cur_module.new_view(cur_frame.ip);
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
					if self.stack.len() > 0 {	
						let cur_frame = &mut self.stack[stack_top-1];
						if let Some(ret_reg) = cur_frame.rslot {
							let val = frame._take_unchecked(r1)?;
							let ret_reg = ret_reg as usize;
							cur_frame.write_unchecked(ret_reg, val, &mut self.refc)?;
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
					let val = cur_module.extern_fref(cur_frame.module_id, param.id as usize);
					cur_frame.write_register(param.r1, val, &mut self.refc)?;
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

					let is_tc = _check_tailcall(&mut slvw, cur_frame.rslot);

					let mut param = Vec::<Option<BaseType>>::new();
					for reg in rparam.regs {
						let rid = cur_frame.get_reg_id(reg)?;
						let val = cur_frame._take_register(rid, &mut self.refc)?;
						param.push(val);
					}

					let val = cur_frame.read_register(dreg.r1)?;
					let (mod_id, func_idx, unext) = _extract_finfo(val)?;

					std::mem::drop(slvw); std::mem::drop(cur_frame); std::mem::drop(cur_module); std::mem::drop(module_pool_vec_read); // Explicit drops for clarity.
					let (mid, fid) = _prep_fcall(&self.pool,mod_id, func_idx, unext)?;
					// eprintln!("call fn, {mid}:{fid} with {param:?}");
					let new_frame = _make_frame(param, &self.pool, mid, fid)?;

					if !is_tc {
						self.stack.push(new_frame);	    // Push a new frame in the normal case.
					} else {
						let idx = self.stack.len() -1;
						self.stack[idx] = new_frame;	// Overwrite frame in case of tail-call.s
					}

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
				op::LENGTH => instr_2arg!(slvw, cur_frame, self.refc, |val| {types::as_composite_type(val, &self.refc)?.length()?}),
				op::BNOT => instr_2arg!(slvw, cur_frame, self.refc, |rval| { match rval {BaseType::Int(i1) => BaseType::Int(!*i1),_ => {return Err(types::new_error(ErrorType::TypeMismatch, format!("Unsupported bitwise not on {rval}")));}}}),
				op::FLOOR => instr_2arg!(slvw, cur_frame, self.refc, |rval| {types::try_floor(rval).ok_or(types::new_error(ErrorType::TypeMismatch, format!("Unsupported 'floor' on {rval}")))?}),
				op::ISN0 => instr_2arg!(slvw, cur_frame, self.refc, |rv| {if types::as_bool(rv) {types::BaseType::Int(1)} else {types::BaseType::Int(0)}}),
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
					let wval = types::as_composite_type_mut(rval, &self.refc)?.pop()?;
					cur_frame.write_register(dreg.r2, wval, &mut self.refc)?;
				},
				op::SLICE => {
					let qreg = op::QuadrupleRegst::try_from(&mut slvw)?;
					let ctype = types::as_composite_type(cur_frame.read_register(qreg.r2)?, &self.refc)?;
					let s1 = cur_frame.read_register(qreg.s1)?;
					let s2 = cur_frame.read_register(qreg.s2)?;
					let val = ctype.slice(s1, s2, &mut self.refc)?;
					cur_frame.write_register(qreg.r1, val, &mut self.refc)?;
				},
				op::GETINDEX => instr_3arg!(types::try_index, slvw, cur_frame, &mut self.refc),
				_ => {
					return Err(new_error(ErrorType::InvalidOpcode, format!("Unrecognized opcode {:x} at offset {}", opcode, cur_frame.ip)));
				}
			}
			cur_frame.ip =slvw.offset();
		}
		Ok(())
	}
}