pub mod exec;
pub mod module;
pub mod op;

mod types;

pub(crate) use types::new_error;
pub(crate) use types::BaseType;
use types::CompositeType;
pub use types::{ErrorType, FatalErr};

pub type FResult<T> = Result<T, FatalErr>;

pub fn list_from_strings<T>(it: T) -> BaseType
where
    T: Iterator<Item = String>,
{
    let v = it
        .map(|s| BaseType::Alloc(Box::new(CompositeType::Str(s))))
        .collect();
    BaseType::Alloc(Box::new(CompositeType::List(v)))
}
