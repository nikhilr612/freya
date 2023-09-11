use std::collections::HashMap;
use core::ffi::c_char;
use crate::types::BaseType;

/// Provides an API for exposing and calling functions from native libraries.

/// Struct to hold info pertaining to native modules to be loaded.
struct NativeInterface {
	/// The full name of the native interface. This is usually the path to the file.
	pub(crate) name: String,
	/// Map of function name to index.
	funcname_map: HashMap<String, usize>,
	/// Vector to index native function infromation, such as number of arguments and function pointers.
	function_decl: Vec<(u8, NativeHandle)>
}

/// Struct to temporarily store information during a native call.
pub struct CallEnv {
	/// Field to store the error code for any possible errors during execution of the native function.
	/// By convention:
	///		1. If this field is set to 0x00 (0), then no errors have occurred during execution.
	///		2. If this field is set to any other value, then error is non-recoverable and error message is displayed (if any).
	error_code: usize,
	/// An optional error message to be displayed.
	error_msg: Option<String>,
	/// The value to be returned to the calling function.
	retval: Option<BaseType>,
	/// A vector of nullable arguments passed during function call.
	param: Vec<Option<BaseType>>
}

impl CallEnv {
	pub(crate) fn new(param: Vec<Option<BaseType>>) -> CallEnv {
		CallEnv {
			error_code: 0,
			error_msg: None,
			retval: None,
			param
		}
	}
}

/// Alias for the function pointer type required to be exported by the native library.
pub type NativeHandle = extern "C" fn (env: &mut CallEnv);

#[inline]
unsafe fn mutptr_to_mref<'a, T>(ptr: *mut T) -> & 'a mut T {
	if ptr.is_null() {
		panic!("fatal: Native Handle received NULL pointer.");
	} else {
		&mut *ptr
	}
}

#[inline]
unsafe fn constptr_to_ref<'a, T>(ptr: *const T) -> & 'a T {
	if ptr.is_null() {
		panic!("fatal: Native Handle received NULL pointer.");
	} else {
		&*ptr
	}
}

#[no_mangle]
/// Set error message for CallEnv
pub unsafe extern "C" fn fr_callenv_seterr(cenv: *mut CallEnv, code: usize, msg: *const c_char) {
	let cenv = mutptr_to_mref(cenv);
	cenv.error_code = code;
	if !msg.is_null() {

	}
}

#[no_mangle]
/// Get an integer positional parameter from CallEnv.
/// - `pos`: The position of the parameter to retrieve value from.
/// - `out`: The mutable i64 pointer into which the retrieved value shall be written.
/// Returns true if, indeed an integer was retrieved, else false;
pub unsafe extern "C" fn fr_getint_param(cenv: *const CallEnv, pos: usize, out: *mut i64) -> bool {
	if out.is_null() {
		return false;
	}
	let cenv = constptr_to_ref(cenv);
	if let Some(bt) = &cenv.param[pos] {
		if let BaseType::Int(i) = bt {
			*out = *i;
			true
		} else {
			false
		}
	} else {
		false
	}
}

#[no_mangle]
/// Get a double-precision floating point positional parameter from CallEnv.
/// - `pos`: The position of the parameter to retrieve the value from.
/// - `out`: The mutable f64 pointer into to which the retrieved value shall be written.
/// Returns true, if a value was retrieved.
pub unsafe extern "C" fn fr_getflt_param(cenv: *const CallEnv, pos: usize, out: *mut f64) -> bool {
	if out.is_null() {
		return false;
	}
	let cenv = constptr_to_ref(cenv);
	if let Some(bt) = &cenv.param[pos] {
		if let BaseType::Flt(v) = bt {
			*out = *v;
			true
		} else {
			false
		}
	} else {
		false
	}
}

#[no_mangle]
/// Get a UTF32 character positional parameter from CallEnv.
/// - `pos`: The position of the parameter to retrieve the value from.
/// - `out`: The mutable u32 pointer into to which the retrieved value shall be written.
/// Returns true, if a value was retrieved.
pub unsafe extern "C" fn fr_getchar_param(cenv: *const CallEnv, pos: usize, out: *mut u32) -> bool {
	if out.is_null() {
		return false;
	}
	let cenv = constptr_to_ref(cenv);
	if let Some(bt) = &cenv.param[pos] {
		if let BaseType::Chr(v) = bt {
			*out = (*v) as u32;
			true
		} else {
			false
		}
	} else {
		false
	}
}

#[no_mangle]
/// Get a pointer (potentially NULL) from the OpaqueHandle type from a positional paramter.
/// Returns NULL if either the parameter is None, or if the parameter is not an OpaqueHandle.
pub unsafe extern "C" fn fr_get_opaque_param(cenv: *const CallEnv, pos: usize) -> *mut core::ffi::c_void {
	let cenv = constptr_to_ref(cenv);
	if let Some(bt) = &cenv.param[pos] {
		if let BaseType::OpaqueHandle(hndl) = bt {
			return (*hndl) as *mut core::ffi::c_void;
		}
	}
	core::ptr::null_mut()
}

#[no_mangle]
/// Set the return value to a UTF32 character.
pub unsafe extern "C" fn fr_set_return_char(cenv: *mut CallEnv, v: u32) {
	let cenv = mutptr_to_mref(cenv);
	cenv.retval = Some(BaseType::Chr(char::from_u32(v).ok_or('\u{fffd}').unwrap()));
}

#[no_mangle]
/// Set the return value to an integer value.
pub unsafe extern "C" fn fr_set_return_int(cenv: *mut CallEnv, v: i64) {
	let cenv = mutptr_to_mref(cenv);
	cenv.retval = Some(BaseType::Int(v));
}

#[no_mangle]
/// Set the return value to a double precision floating point value.
pub unsafe extern "C" fn fr_set_return_flt(cenv: *mut CallEnv, v: f64) {
	let cenv = mutptr_to_mref(cenv);
	cenv.retval = Some(BaseType::Flt(v));
}

#[no_mangle]
/// Set the return value to an OpaqueHandle type.
pub unsafe extern "C" fn fr_set_return_opaque(cenv: *mut CallEnv, ptr: *const core::ffi::c_void) {
	let cenv = mutptr_to_mref(cenv);
	cenv.retval = Some(BaseType::OpaqueHandle(ptr as usize))
}