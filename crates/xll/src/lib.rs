#![cfg(windows)]
#![allow(clippy::missing_safety_doc)]

mod args;
mod callback;
mod callback_old;
pub mod dyncall;
mod host;
mod macro_table;
pub mod sig;
mod xlfn;
pub mod xloper;
mod xloper_old;

pub use callback::MdCallBack12;
pub use callback_old::MdCallBack;
pub use host::{XllError, XllHost};
