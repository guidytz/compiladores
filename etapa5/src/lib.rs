use std::cell::RefCell;

use cfgrammar::Span;
use errors::ParsingError;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;
use semantic_aux::{check_global, ScopeStack, SymbolEntry};

pub mod ast;
pub mod errors;
pub mod iloc_aux;
pub mod semantic_aux;

thread_local!(pub static SCOPE_STACK: RefCell<ScopeStack> = RefCell::new(ScopeStack::new()));
thread_local!(pub static TEMP_COUNTER: RefCell<u32> = RefCell::new(0));
thread_local!(pub static LABEL_COUNTER: RefCell<u32> = RefCell::new(0));

pub fn add_symbol_to_curr_st(_symbol_entry: SymbolEntry) -> Result<(), ParsingError> {
    #[cfg(feature = "semantics")]
    SCOPE_STACK.with(|stack| stack.borrow_mut().add_symbol(_symbol_entry))?;
    Ok(())
}

pub fn new_scope() {
    #[cfg(feature = "semantics")]
    SCOPE_STACK.with(|stack| stack.borrow_mut().new_scope());
}

pub fn end_scope() {
    #[cfg(feature = "semantics")]
    SCOPE_STACK.with(|stack| stack.borrow_mut().pop_scope());
}

pub fn get_symbol(
    _span: Span,
    _lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
) -> Result<SymbolEntry, ParsingError> {
    #[cfg(feature = "semantics")]
    return SCOPE_STACK.with(|stack| stack.borrow().get_symbol(_span, _lexer));

    #[cfg(not(feature = "semantics"))]
    Ok(SymbolEntry::None)
}

pub fn clear_stack() {
    #[cfg(feature = "semantics")]
    SCOPE_STACK.with(|stack| stack.borrow_mut().clear());
}

pub fn get_new_temp() -> String {
    let temp_val = TEMP_COUNTER.with(|counter| counter.borrow().clone());
    TEMP_COUNTER.with(|counter| *counter.borrow_mut() += 1);

    format!("r{temp_val}")
}

pub fn get_new_label() -> String {
    let temp_val = LABEL_COUNTER.with(|counter| counter.borrow().clone());
    LABEL_COUNTER.with(|counter| *counter.borrow_mut() += 1);

    format!("L{temp_val}")
}

pub fn get_reg(symbol: &SymbolEntry) -> String {
    match check_global(symbol) {
        true => "rbss".to_string(),
        false => "rfp".to_string(),
    }
}
