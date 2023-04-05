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
    SCOPE_STACK.with(|stack| stack.borrow_mut().get_reg())
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

#[cfg(feature = "code")]
pub fn get_fn_label(_name: String) -> Result<String, ParsingError> {
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

#[cfg(feature = "code")]
pub fn save_regs() -> Vec<asm_aux::AsmInst> {
    new_reg_temps()
        .into_iter()
        .map(|reg| {
            let reg = if reg.contains("e") {
                reg.replace("e", "r")
            } else {
                reg.replace("d", "")
            };

            asm_aux::AsmInst::StackInst(asm_aux::StackInst::new("pushq".to_string(), reg))
        })
        .collect()
}

#[cfg(feature = "code")]
pub fn restore_regs() -> Vec<asm_aux::AsmInst> {
    new_reg_temps()
        .into_iter()
        .rev()
        .map(|reg| {
            let reg = if reg.contains("e") {
                reg.replace("e", "r")
            } else {
                reg.replace("d", "")
            };

            asm_aux::AsmInst::StackInst(asm_aux::StackInst::new("popq".to_string(), reg))
        })
        .collect()
}

pub fn new_reg_temps() -> Vec<&'static str> {
    vec![
        "%ebx", "%ecx", "%edx", "%esi", "%edi", "%r9d", "%r10d", "%r11d", "%r12d", "%r13d",
        "%r14d", "%r15d",
    ]
}
