use std::sync::RwLockWriteGuard;

use std::sync::RwLockReadGuard;
use std::sync::RwLock;
use crate::utils::SliceView;
use crate::exec::RefCounter;
use crate::types::CompositeType;
use crate::types::BaseType;
use crate::types::ErrorType;
use crate::types::FatalErr;

use memmap2::Mmap;
use std::fs::File;
use std::collections::HashMap;

pub const MAGIC: [u8; 4] = [0x3e, 0xb3, 0x14, 0x63];
pub const FILEXT: &str = ".fr";
pub type FResult<T> = Result<T, FatalErr>;

#[derive(Debug)]
pub enum ResolutionStatus {
	Resolved,
	Unresolved,
	Native
}

#[derive(Debug)]
pub struct ExternDecl {
	/// Path to module specified in extern declaration.
	pub(crate) module_path: String,
	/// Function name specified in extern declaration.
	pub(crate) func_name: String,
	/// Enum to check if extern is resolved/unresolved or if it points to a native function.
	pub status: ResolutionStatus,
	/// The id of the module
	pub(crate) module_id: usize,
	/// The id of the function within the module.
	pub(crate) function_id: usize
}

impl ExternDecl {
	fn new_extern(mpath: &str, fn_name: &str) -> ExternDecl {
		let mut mp = mpath.to_owned();
		mp.push_str(FILEXT);
		ExternDecl {
			module_path: mp,
			func_name: fn_name.to_owned(),
			module_id: 0,
			function_id: 0,
			status: ResolutionStatus::Unresolved
		}
	}

	#[inline]
	pub fn resolved(&self) -> bool {
		match &self.status {
			ResolutionStatus::Resolved => true,
			_ => false
		}
	}
}

#[derive(Debug)]
pub struct FuncDecl {
	pub(crate) nparam: u8,
	pub(crate) nregs: u8,
	pub(crate) offset: u32
}

/// Struct to hold info pertaining to module
#[derive(Debug)]
pub struct Module {
	/// The full name of the module. This is usually the path of the module.
	pub(crate) name: String,
	/// Vector of all constants in constant pool.
	constant_pool: Vec<BaseType>,
	/// Map of function name to index.
	funcname_map: HashMap<String, usize>,
	/// Vector to index function declaration information
	function_decl: Vec<FuncDecl>,
	/// Vector to index extern declarations.
	extern_decl: Vec<ExternDecl>,
	code: Mmap
}

 impl Module {
 	pub fn fdecl_by_id(&self, id: usize) -> &FuncDecl {
 		&self.function_decl[id]
 	}

 	pub fn name_by_id(&self, id: usize) -> Option<&String> {
 		self.funcname_map.iter().find_map(|(key, &val)| if val == id { Some(key) } else { None })
 	}

 	pub(crate) fn new_view(&self, off: usize) -> SliceView {
 		SliceView::wrap(off, &self.code)
 	}

 	pub fn func_id(&self, n: &str) -> FResult<usize> {
 		let idx = self.funcname_map.get(n).ok_or(
 			crate::types::new_error(
 				ErrorType::NoSuchFunction, 
 				format!("Function {n} not part of loaded module {},\n\t {self:#?}", self.name)
 			)
 		)?;
 		Ok(*idx)	
 	}

 	pub(crate) fn extern_fref(&self, mid2: usize, id1: usize) -> crate::types::BaseType {
 		let extd = &self.extern_decl[id1 % self.extern_decl.len()];
 		if extd.resolved() {
 			crate::types::CompositeType::new_fref(extd.module_id, extd.function_id, false)
 		} else {
 			crate::types::CompositeType::new_fref(mid2, id1, true)
 		}
 	}

 	pub fn extern_ref(&self, id: usize) -> &ExternDecl {
 		&self.extern_decl[id]
 	}

 	pub fn extern_mutref(&mut self, id: usize) -> &mut ExternDecl {
 		&mut self.extern_decl[id]
 	}

 	pub(crate) fn clone_constant(&self, idx: usize, wt: &mut RefCounter) -> Option<BaseType> {
 		match &self.constant_pool[idx] {
 			BaseType::Int(i1) => Some(BaseType::Int(*i1)),
 			BaseType::Flt(f1) => Some(BaseType::Flt(*f1)),
 			BaseType::Chr(ch) => Some(BaseType::Chr(*ch)),
 			BaseType::Alloc(bx) => {
 				let ptr = bx.as_ref() as *const CompositeType;
 				wt.incref(ptr).map_err(|e| {eprintln!("massive cockup: {}\n\tAn object from the constant pool must only have immutable references. Anything else reeks of blunder.", e);}).unwrap();
 				Some(BaseType::ConstRef(ptr)) 				
 			},
 			_ => None
 		}
 	}
 }

/// Struct for loading multiple modules.
/// Modules cannot be selectively dropped.
/// Ideally, modules should only be loaded on the first call
 #[derive(Debug)]
pub struct ModulePool {
	/// RwLock for vector of loaded modules
 	mlock: RwLock<Vec<Module>>,
 	/// Path map for modules
 	pathm: RwLock<HashMap<String, usize>>,
 }

pub type PoolReadGuard<'a> = RwLockReadGuard<'a, Vec<Module>>;
pub type PoolWriteGuard<'a> = RwLockWriteGuard<'a, Vec<Module>>;

// TODO: Re-implement. Bad design. RwLock locks indefinitely.
 impl ModulePool {
 	pub fn new() -> ModulePool {
 		ModulePool {
 			mlock: RwLock::new(Vec::new()),
 			pathm: RwLock::new(HashMap::new())
 		}
 	}

 	pub fn load(&self, path: &str) -> FResult<()> {
 		let mut pbuf = std::env::current_dir().map_err(|e| {
 			let m = format!("Failed to create path to module {path}, cause {e:?}");
 			crate::types::new_error(ErrorType::ModuleLoadFailure, m)
 		})?;
 		pbuf.push(path);
   		let modules = &mut self.mlock.write().expect("Failed to acquire write lock for module vec.");

 		let module = open(pbuf.to_str().unwrap())?;
 		modules.push(module);

 		let pathmp = &mut self.pathm.write().expect("Failed to acquire write lock for path map.");
 		pathmp.insert(path.to_string(), modules.len()-1);

 		return Ok(())
 	}

 	/// Return the id of a module
 	pub fn id_by_name(&self, path: &str) -> FResult<usize> {
 		let p = self.pathm.read().expect("Failed to acquire read lock for path map.");
 		let idx = match p.get(path) {
 			Some(i) => *i,
 			None => {
 				return Err(crate::types::new_error(ErrorType::NoSuchModule, format!("Module path: {path} has not been loaded into the pool.")));
 			}
 		};
 		return Ok(idx);
 	}

 	pub fn read_lock<'a>(&self) -> PoolReadGuard {
 		self.mlock.read().expect("Failed to acquire read lock for module vec.")
 	}

 	pub fn is_loaded(&self, path: &str) -> bool {
 		self.pathm.read().unwrap().contains_key(path)
 	}

 	pub fn write_lock(&self) -> PoolWriteGuard {
 		self.mlock.write().expect("Failed to acquire write lock for module vec.")
 	}

 	pub fn update_path(&self, path: &str, id: usize) {
 		let pathmp = &mut self.pathm.write().expect("Failed to acquire write lock for path map.");
 		pathmp.insert(path.to_string(), id);
 	}
 }

/// Open a module with the given path.
/// ## Format:
/// 1. MAGIC - header starts with 0x3E B3 14 63 (4 byte sequence)
/// 2. Size of pool (u32)
/// 3. Type identifier, type data.
/// 4. Function declarations.
/// Note: Strings can be stored, despite not being a primitive type. However, only constant-refs are allowed.
/// Constant Pool specification: `len: u16, {type_id, data}*len`
/// 1 => i64,
/// 2 => f64,
/// 3 => char,
/// 4 => utf8_str
pub fn open(fpath: &str) -> FResult<Module> {
	let f = match File::open(fpath) {
		Ok(f) => f,
		Err(_e) => {
			return Err(crate::types::new_error(ErrorType::ModuleLoadFailure, format!("Couldn't open module with path: {}", fpath)));
		}
	};
	let mmap = unsafe {
		Mmap::map(&f)
	};
	let mmap = match mmap {
		Ok(m) => m, 
		Err(_e) => {
			return Err(crate::types::new_error(ErrorType::ModuleLoadFailure, format!("Couldn't create mmap for path: {}", fpath)));
		}
	};
	let mut view = SliceView::wrap(0, &mmap);
	if view.take(4) != MAGIC {
		return Err(crate::types::new_error(ErrorType::ModuleLoadFailure, format!("Illegal header start sequence for file: {}", fpath)));
	}
	let mut cpool = Vec::new();
	let vlen = view.get_u16() as usize;
	for _i in 0..vlen {
		match view.get_u8() {
			1 => {
				cpool.push(BaseType::Int(view.get_u64() as i64));
			},
			2 => {
				let val = f64::from_bits(view.get_u64());
				cpool.push(BaseType::Flt(val));
			},
			3 => {
				let rval = view.get_u32();
				let val = match char::from_u32(rval) {
					Some(ch) => ch,
					None => {
						return Err(crate::types::new_error(ErrorType::UtfDecodeError, format!("Value {:x} is not a valid Unicode scalar", rval)));
					}
				};
				cpool.push(BaseType::Chr(val));
			},
			4 => {
				let len = view.get_u16() as usize;
				let s = view.decode_utf8(len)?;
				let bx = Box::new(CompositeType::Str(s));
				cpool.push(BaseType::Alloc(bx));
			},
			_ => {
				return Err(crate::types::new_error(ErrorType::ModuleLoadFailure, "Invalid type id for constant pool"));
			}
		}
	}
	let elen = view.get_u16() as usize;
	let mut edecs = Vec::new();
	for _i in 0..elen {
		let nlen = view.get_u16() as usize;
		let st = view.decode_utf8(nlen)?;
		let (mpath, fname) = match st.split_once(':') {
			Some(tup) => tup,
			None => {
				return Err(crate::types::new_error(ErrorType::ModuleLoadFailure, format!("Invalid extern path {st}")))
			}
		};
		let ext = ExternDecl::new_extern(mpath, fname);
		edecs.push(ext);

	}
	let vlen = view.get_u16() as usize;
	let mut fdecs = Vec::new();
	let mut fmap = HashMap::new();
	for _i in 0..vlen {
		let nlen = view.get_u8() as usize;
		let st = view.decode_utf8(nlen)?;
		let np = view.get_u8();
		let nl = view.get_u8();
		let off = view.get_u32();
		fdecs.push(FuncDecl{nparam: np, nregs: nl, offset: off});
		fmap.insert(st, fdecs.len() -1);
	}
	let ret = Module {
		name: fpath.to_owned(),
		constant_pool: cpool,
		funcname_map: fmap,
		function_decl: fdecs,
		extern_decl: edecs,
		code: mmap
	};
	Ok(ret)
}