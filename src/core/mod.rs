pub mod module;
pub mod exec;
pub mod op;

mod types; 

pub use types::ErrorType;
pub use types::FatalErr;
pub(crate) use types::new_error;
pub(crate) use types::BaseType;