//! Implements the storing and loading of native interfaces.

use std::path::Path;
use std::collections::HashMap;
use libloading::{Library, Symbol};

use crate::core::{new_error, ErrorType, FResult};

use super::NativeHandle;

/// Struct to hold info pertaining to native modules loaded.
/// NativeInterfaces are only loaded upon the first call to a function.
/// Functions are loaded upon their first invocation. Native functions are not pre-resolved.
#[derive(Default)]
struct NativeInterface {
	/// Map of function name to index.
	funcname_map: HashMap<String, usize>,
	/// Vector of native functions loaded.
	functions: Vec<NativeHandle>
}

#[derive(Default)]
pub struct InterfacePool {
	/// Map library names to shared library path.
	libpaths: HashMap<String, String>,
	/// Vector of all native interfaces loaded.
	ifaces: Vec<NativeInterface>,
	/// Map library names to indices.
	indices: HashMap<String, usize>,
	libs: Vec<Library>
}

impl InterfacePool {
	fn add_path(&mut self, name: &str, path: &str) {
		if Path::new(path).exists() {
			self.libpaths.insert(name.to_string(), path.to_string());
		} else {
			eprintln!("WARNING: Native path {path} for {name} does not exist.");
		}
	}

	fn lib_id(&mut self, name: &str) -> FResult<usize> {
		if let Some(idx) = self.indices.get(name) {		// Return if it's already loaded.
			return Ok(*idx);
		}

		// It's not, so check if it's actually a library specified.
		if let Some(lpath) = self.libpaths.get(name){
			let lib = unsafe { Library::new(lpath) }.map_err(|e| 
				new_error(ErrorType::NativeLoadFailure, format!("Failed to load native lib \'{name}\' [from {lpath}], cause: {e:?}"))
			)?;
			let ret = self.libs.len();
			self.indices.insert(name.to_string(), ret);
			self.ifaces.push(NativeInterface::default());
			self.libs.push(lib);
			Ok(ret)
		} else {	// Don't allow random libraries to be loaded / initialized.
			Err(new_error(ErrorType::UnknownLibrary, format!("Cannot load unknown shared library \'{name}\'.")))
		}
	}

	/// Get a native handle of a function from a library, provided they have already been loaded.
	fn handle_by_index(&self, lib_id: usize, f_id: usize) -> FResult<NativeHandle> {
		self.ifaces.get(lib_id).and_then(|a| a.functions.get(f_id)).copied().ok_or_else(|| {
			new_error(ErrorType::NoSuchFunction, format!("Failed to retrieve native handle #{f_id} from lib{lib_id}"))
		})
	}

	/// Load a native handle from a library.
	/// If the library is known, but not loaded, load it.
	/// Having loaded the native handle, append it to the associated interface.
	/// Returns the library id, function id of the handle within the corresponding native interface, and the native handle itself.
	fn load_handle(&mut self, libname: &str, fname: &str) -> FResult<(usize, usize, NativeHandle)> {
		let lid = self.lib_id(libname)?;
		let niface = &mut self.ifaces[lid];

		if let Some(&index) = niface.funcname_map.get(fname) {	// If function is already loaded, just return it.
			return Ok((lid, index, niface.functions[index]));
		}

		let handle = unsafe { self.libs.get(lid).unwrap().get(fname.as_bytes()).map(|s: Symbol<NativeHandle>| *s) }.map_err(|e| {
			new_error(ErrorType::NoSuchSymbol, format!("Could not obtain symbol \'{fname}\' from \'{libname}\', cause: {e:?}"))
		})?;
		let fid = niface.functions.len();
		niface.funcname_map.insert(fname.to_string(), fid);
		niface.functions.push(handle);
		Ok((lid, fid, handle))
	}
}