use std::cell::RefCell;

use cfgrammar::Span;
use errors::ParsingError;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;
use semantic_aux::{check_global, ScopeStack, ScopeType, SymbolEntry};

pub mod asm_aux;
pub mod ast;
pub mod errors;
pub mod semantic_aux;

thread_local!(pub static SCOPE_STACK: RefCell<ScopeStack> = RefCell::new(ScopeStack::new()));
thread_local!(pub static TEMP_COUNTER: RefCell<Vec<String>> = RefCell::new(vec![
    "%rax".to_string(), "%rbx".to_string(), "%rcx".to_string(), "%rdx".to_string(),
    "%rsi".to_string(), "%rdi".to_string(),
    "%r9d".to_string(), "%r10d".to_string(), "%r11d".to_string(), "%r12d".to_string(), "%r13d".to_string(), "%r14d".to_string(), "%r15d".to_string(),
]));
thread_local!(pub static LABEL_COUNTER: RefCell<u32> = RefCell::new(0));

pub fn add_symbol_to_curr_st(_symbol_entry: SymbolEntry) -> Result<(), ParsingError> {
    #[cfg(feature = "semantics")]
    SCOPE_STACK.with(|stack| stack.borrow_mut().add_symbol(_symbol_entry))?;
    Ok(())
}

pub fn add_name_to_last_child_table(_name: String) {
    #[cfg(feature = "semantics")]
    SCOPE_STACK.with(|stack| stack.borrow_mut().add_name_to_last_child_table(_name));
}

pub fn new_scope(_scope_type: ScopeType) {
    #[cfg(feature = "semantics")]
    SCOPE_STACK.with(|stack| stack.borrow_mut().new_scope(_scope_type));
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

pub fn get_new_temp() -> Result<String, ParsingError> {
    let temp_val = TEMP_COUNTER.with(|counter| counter.borrow_mut().pop());

    match temp_val {
        Some(temp_val) => Ok(temp_val),
        None => Err(ParsingError::NoMoreRegisters),
    }
}

pub fn get_new_label() -> String {
    let temp_val = LABEL_COUNTER.with(|counter| counter.borrow().clone());
    LABEL_COUNTER.with(|counter| *counter.borrow_mut() += 1);

    format!(".L{temp_val}")
}

pub fn get_val(symbol: &SymbolEntry) -> String {
    match check_global(symbol) {
        true => {
            let name = symbol.name();
            format!("{name}(%rip)")
        }
        false => {
            let desloc = symbol.desloc();
            format!("-{desloc}(%rbp)")
        }
    }
}

pub fn change_base_function_desloc(_args_size: u32) {
    #[cfg(feature = "semantics")]
    SCOPE_STACK.with(|stack| stack.borrow_mut().change_base_function_desloc(_args_size))
}

pub fn get_fn_label(_name: String) -> Result<String, ParsingError> {
    #[cfg(feature = "code")]
    return SCOPE_STACK.with(|stack| {
        let label = stack
            .borrow()
            .0
            .first()
            .ok_or(ParsingError::NoScope)?
            .get(&_name)
            .ok_or(ParsingError::ErrUndeclared(_name))?
            .get_label();
        Ok(label)
    });

    #[cfg(not(feature = "code"))]
    Ok("".to_string())
}

pub fn get_fn_size(_name: String) -> Result<u32, ParsingError> {
    #[cfg(feature = "code")]
    return SCOPE_STACK.with(|stack| stack.borrow_mut().get_fn_size(_name));

    #[cfg(not(feature = "code"))]
    Ok(0)
}

pub fn get_var_deslocs(_name: String) -> Result<Vec<(String, u32)>, ParsingError> {
    #[cfg(feature = "code")]
    return SCOPE_STACK.with(|stack| stack.borrow_mut().get_var_deslocs(_name));

    #[cfg(not(feature = "code"))]
    Ok(Vec::new())
}
