pub mod module;
pub mod exec;
pub mod op;

mod types; 

pub use types::{ErrorType, FatalErr};
pub(crate) use types::new_error;
pub(crate) use types::BaseType;
use types::CompositeType;

pub type FResult<T> = Result<T, FatalErr>;

pub fn list_from_strings<T>(it: T) -> BaseType
where T: Iterator<Item = String> {
	let v = it
			.map(|s| BaseType::Alloc(Box::new(CompositeType::Str(s))))
			.collect();
	BaseType::Alloc(Box::new(CompositeType::List(v)))
}