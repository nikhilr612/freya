use crate::core::exec::RefCounter;
use crate::core::types::{self, new_error, BaseType, CompositeType, ErrorType};
use crate::core::FResult;
use crate::utils::SliceView;
use std::path::{Path, PathBuf};
use std::sync::{RwLock, RwLockReadGuard, RwLockWriteGuard};

use memmap2::Mmap;
use std::collections::HashMap;
use std::fs::File;

pub const MAGIC: [u8; 4] = [0x3e, 0xb3, 0x14, 0x63];
#[cfg(windows)]
pub const LIBRARYEXT: &str = "dll";
#[cfg(unix)]
pub const LIBRARYEXT: &str = "so";

pub const FILEXT: &str = "fr";
#[derive(Debug)]
#[repr(u8)]
/// Enum to denote state of extern declarations.
pub enum ResolutionStatus {
    /// An extern-decl in resolved, iff the module being referred to is loaded and the extern has been 'touched' alteast once.
    Resolved,
    /// An extern-decl is unresolved, until it is 'touched' whereupon the status is changed to 'resolved'.
    /// The actual module externed may be loaded into the pool already; if it is, the function is located, otherwise the module is loaded.
    Unresolved,
    //Native
}

#[derive(Debug)]
/// Alternative value enum.
/// Convert `BaseType to `ConstantValue` to send value across threads; and explicitly requires verification of the entire object tree.
/// `ConstantValue` -> `BaseType is a trivial copy, but `BaseType -> `ConstantValue` is a tree search.
/// Used to ensure that BaseType is explicitly not Send or Sync.
pub(crate) enum ConstantValue {
    Int(i64),
    Flt(f64),
    Chr(char),
    /// The only object allowed in constant pool is heap-allocated strings.
    Obj(Box<CompositeType>),
}

unsafe impl Send for ConstantValue {}
unsafe impl Sync for ConstantValue {}

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
    pub(crate) function_id: usize,
    pub(crate) native: bool,
}

impl ExternDecl {
    pub fn new_extern(mpath: &str, fn_name: &str, native: bool) -> ExternDecl {
        ExternDecl {
            module_path: mpath.to_owned(),
            func_name: fn_name.to_owned(),
            module_id: 0,
            function_id: 0,
            status: ResolutionStatus::Unresolved,
            native,
        }
    }
}

#[derive(Debug)]
pub struct FuncDecl {
    pub(crate) nparam: u8,
    pub(crate) nregs: u8,
    pub(crate) offset: u32,
}

/// Struct to hold info pertaining to module
#[derive(Debug)]
pub struct Module {
    /// The full name of the module. This is usually the path of the module.
    pub(crate) name: String,
    /// Vector of all constants in constant pool.
    constant_pool: Vec<ConstantValue>,
    /// Map of function name to index.
    funcname_map: HashMap<String, usize>,
    /// Vector to index function declaration information
    function_decl: Vec<FuncDecl>,
    /// Vector to index extern declarations.
    extern_decl: Vec<ExternDecl>,
    code: Mmap,
}

impl Module {
    pub fn fdecl_by_id(&self, id: usize) -> &FuncDecl {
        &self.function_decl[id]
    }

    pub fn name_by_id(&self, id: usize) -> Option<&String> {
        self.funcname_map
            .iter()
            .find_map(|(key, &val)| if val == id { Some(key) } else { None })
    }

    pub(crate) fn new_view(&self, off: usize) -> SliceView {
        SliceView::wrap(off, &self.code)
    }

    pub fn func_id(&self, n: &str) -> FResult<usize> {
        let idx = self.funcname_map.get(n).ok_or(types::new_error(
            ErrorType::NoSuchFunction,
            format!(
                "Function {n} not part of loaded module {},\n\t {self:#?}",
                self.name
            ),
        ))?;
        Ok(*idx)
    }

    pub fn extern_ref(&self, id: usize) -> &ExternDecl {
        &self.extern_decl[id]
    }

    pub fn extern_mutref(&mut self, id: usize) -> &mut ExternDecl {
        &mut self.extern_decl[id]
    }

    pub(crate) fn clone_constant(&self, idx: usize, wt: &mut RefCounter) -> BaseType {
        match &self.constant_pool[idx] {
            ConstantValue::Int(i1) => BaseType::Int(*i1),
            ConstantValue::Flt(f1) => BaseType::Flt(*f1),
            ConstantValue::Chr(ch) => BaseType::Chr(*ch),
            ConstantValue::Obj(bx) => {
                let ptr = bx.as_ref() as *const CompositeType;
                wt.incref(ptr).map_err(|e| {
                    eprintln!("massive cockup: {e}\n\tAn object from the constant pool must only have immutable references. Anything else reeks of blunder.");
                }).unwrap();
                BaseType::ConstRef(ptr)
            }
        }
    }
}

/// Struct for loading multiple modules.
/// Modules cannot be selectively dropped.
/// Ideally, modules should only be loaded on the first call
#[derive(Debug)]
pub struct ModulePool {
    /// Lock for vector of loaded modules
    mlock: RwLock<Vec<Module>>,
    /// Path map for modules
    pathm: RwLock<HashMap<String, usize>>,
    /// Paths to search for modules.
    spath: Vec<Box<Path>>,
}

pub type PoolReadGuard<'a> = RwLockReadGuard<'a, Vec<Module>>;
pub type PoolWriteGuard<'a> = RwLockWriteGuard<'a, Vec<Module>>;

// TODO: Re-implement. Bad design. RwLock locks indefinitely.
impl ModulePool {
    pub fn new() -> ModulePool {
        ModulePool {
            mlock: RwLock::new(Vec::new()),
            pathm: RwLock::new(HashMap::new()),
            spath: Vec::new(),
        }
    }

    pub fn add_path(&mut self, path: &Path) {
        if path.exists() {
            self.spath.push(path.into());
        } // Don't care about paths that don't exist.
    }

    pub fn resolve_path(&self, path: &str) -> FResult<PathBuf> {
        let mut buf = PathBuf::new();
        for dirpath in &self.spath {
            buf.push(dirpath);
            buf.push(Path::new(path));
            buf.set_extension(FILEXT);
            if buf.as_path().exists() {
                return Ok(buf);
            }
        }
        Err(types::new_error(
            ErrorType::NoSuchModule,
            format!("Module or library {path} not found in available paths."),
        ))
    }

    pub fn load(&mut self, path: &str) -> FResult<()> {
        let pbuf = self.resolve_path(path)?;

        let modules = self.mlock.get_mut().map_err(|e| {
            new_error(
                ErrorType::InternalFailure,
                format!("ModulePool RwLock 'mlock' is poisoned\n\tcause: {e}"),
            )
        })?;
        let module = open(pbuf.to_str().unwrap())?;
        modules.push(module);

        let pathmp = self.pathm.get_mut().map_err(|e| {
            new_error(
                ErrorType::InternalFailure,
                format!("ModulePool RwLock 'pathm' is poisoned\n\tcause: {e}"),
            )
        })?;
        pathmp.insert(path.to_string(), modules.len() - 1);

        Ok(())
    }

    /// Return the id of a module
    pub fn id_by_name(&self, path: &str) -> FResult<usize> {
        let p = self
            .pathm
            .read()
            .expect("Failed to acquire read lock for path map.");
        let idx = match p.get(path) {
            Some(i) => *i,
            None => {
                return Err(types::new_error(
                    ErrorType::NoSuchModule,
                    format!("Module path: {path} has not been loaded into the pool."),
                ));
            }
        };
        Ok(idx)
    }

    pub fn read_lock(&self) -> PoolReadGuard {
        self.mlock
            .read()
            .expect("Failed to acquire read lock for module vec.")
    }

    /*pub fn is_loaded(&self, path: &str) -> bool {
        self.pathm.read().unwrap().contains_key(path)
    }*/

    pub fn write_lock(&self) -> PoolWriteGuard {
        self.mlock
            .write()
            .expect("Failed to acquire write lock for module vec.")
    }

    pub fn update_path(&self, path: &str, id: usize) {
        let pathmp = &mut self
            .pathm
            .write()
            .expect("Failed to acquire write lock for path map.");
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
            return Err(types::new_error(
                ErrorType::ModuleLoadFailure,
                format!("Couldn't open module with path: {}", fpath),
            ));
        }
    };

    let mmap = unsafe { Mmap::map(&f) };

    let mmap = match mmap {
        Ok(m) => m,
        Err(_e) => {
            return Err(types::new_error(
                ErrorType::ModuleLoadFailure,
                format!("Couldn't create mmap for path: {}", fpath),
            ));
        }
    };

    let mut view = SliceView::wrap(0, &mmap);

    if view.take(4) != MAGIC {
        return Err(types::new_error(
            ErrorType::ModuleLoadFailure,
            format!("Illegal header start sequence for file: {}", fpath),
        ));
    }

    let mut cpool = Vec::new();
    let vlen = view.get_u16() as usize;
    for _i in 0..vlen {
        match view.get_u8() {
            1 => {
                cpool.push(ConstantValue::Int(view.get_u64() as i64));
            }
            2 => {
                let val = f64::from_bits(view.get_u64());
                cpool.push(ConstantValue::Flt(val));
            }
            3 => {
                let rval = view.get_u32();
                let val = match char::from_u32(rval) {
                    Some(ch) => ch,
                    None => {
                        return Err(types::new_error(
                            ErrorType::UtfDecodeError,
                            format!("Value {:x} is not a valid Unicode scalar", rval),
                        ));
                    }
                };
                cpool.push(ConstantValue::Chr(val));
            }
            4 => {
                let len = view.get_u16() as usize;
                let s = view.decode_utf8(len)?;
                let bx = Box::new(CompositeType::Str(s));
                cpool.push(ConstantValue::Obj(bx));
            }
            _ => {
                return Err(types::new_error(
                    ErrorType::ModuleLoadFailure,
                    "Invalid type id for constant pool",
                ));
            }
        }
    }
    let elen = view.get_u16() as usize;
    let mut edecs = Vec::new();
    for _i in 0..elen {
        let native = view.get_u8() != 0;
        let nlen = view.get_u16() as usize;
        let st = view.decode_utf8(nlen)?;
        let (mpath, fname) = match st.split_once(':') {
            Some(tup) => tup,
            None => {
                return Err(types::new_error(
                    ErrorType::ModuleLoadFailure,
                    format!("Invalid extern path {st}"),
                ))
            }
        };
        let ext = ExternDecl::new_extern(mpath, fname, native);
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
        fdecs.push(FuncDecl {
            nparam: np,
            nregs: nl,
            offset: off,
        });
        fmap.insert(st, fdecs.len() - 1);
    }
    let ret = Module {
        name: fpath.to_owned(),
        constant_pool: cpool,
        funcname_map: fmap,
        function_decl: fdecs,
        extern_decl: edecs,
        code: mmap,
    };
    Ok(ret)
}
