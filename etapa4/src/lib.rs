use std::cell::RefCell;

use semantic_aux::ScopeStack;

pub mod ast;
pub mod errors;
pub mod semantic_aux;

thread_local!(pub static SCOPE_STACK: RefCell<ScopeStack> = RefCell::new(ScopeStack::new()));
