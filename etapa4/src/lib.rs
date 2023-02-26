use std::cell::RefCell;

use cfgrammar::Span;
use errors::ParsingError;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;
use semantic_aux::{ScopeStack, SymbolEntry};

pub mod ast;
pub mod errors;
pub mod semantic_aux;

thread_local!(pub static SCOPE_STACK: RefCell<ScopeStack> = RefCell::new(ScopeStack::new()));

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
