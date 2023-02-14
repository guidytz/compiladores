use std::collections::HashMap;

use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

use crate::errors::ParsingError;

#[derive(Debug, Clone)]
pub enum SymbolEntry {
    LitInt(SymbolLitInt),
    LitFloat(SymbolLitFloat),
    LitChar(SymbolLitChar),
    LitBool(SymbolLitBool),
}

impl SymbolEntry {
    pub fn get_line_col(&self) -> (usize, usize) {
        match self {
            SymbolEntry::LitInt(content) => (content.line, content.col),
            SymbolEntry::LitFloat(content) => (content.line, content.col),
            SymbolEntry::LitChar(content) => (content.line, content.col),
            SymbolEntry::LitBool(content) => (content.line, content.col),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolLitInt {
    pub line: usize,
    pub col: usize,
    pub size: u8,
    pub val: u32,
}

#[derive(Debug, Clone)]
pub struct SymbolLitFloat {
    pub line: usize,
    pub col: usize,
    pub size: u8,
    pub val: f64,
}

#[derive(Debug, Clone)]
pub struct SymbolLitChar {
    pub line: usize,
    pub col: usize,
    pub size: u8,
    pub val: char,
}

#[derive(Debug, Clone)]
pub struct SymbolLitBool {
    pub line: usize,
    pub col: usize,
    pub size: u8,
    pub val: bool,
}

#[derive(Debug, Clone)]
pub enum SymbolType {
    CHAR,
    INT,
    FLOAT,
    BOOL,
}

#[repr(transparent)]
pub struct SymbolTable(pub HashMap<String, SymbolEntry>);

impl SymbolTable {
    pub fn new() -> Self {
        Self(HashMap::default())
    }

    pub fn add_symbol(&mut self, key: String, symbol: SymbolEntry) -> Result<(), ParsingError> {
        if let Some(symbol) = self.0.get(&key) {
            let (line, col) = symbol.get_line_col();
            return Err(ParsingError::ErrDeclared(format!(
                "variable with name {key} was first declared at line {}, col {}.",
                line, col
            )));
        }

        self.0.insert(key, symbol);

        Ok(())
    }

    pub fn get(&self, key: &String) -> Option<&SymbolEntry> {
        self.0.get(key)
    }
}

#[repr(transparent)]
pub struct ScopeStack(pub Vec<SymbolTable>);

impl ScopeStack {
    pub fn new() -> Self {
        Self(vec![])
    }

    pub fn get_symbol(&self, key: String, _span: Span) -> Result<SymbolEntry, ParsingError> {
        for table in self.0.iter().rev() {
            match table.get(&key) {
                Some(symbol) => return Ok(symbol.clone()),
                None => continue,
            }
        }

        Err(ParsingError::ErrDeclared(format!(
            "variable with name {key} was never declared"
        )))
    }

    pub fn new_scope(&mut self) {
        self.0.push(SymbolTable::new());
    }

    pub fn pop_scope(&mut self) {
        self.0.pop();
    }
}

#[derive(Debug)]
pub struct UniVar {
    pub line: usize,
    pub col: usize,
    pub name: String,
}

impl UniVar {
    pub fn new(span: Span, lexer: &dyn NonStreamingLexer<DefaultLexerTypes>) -> Self {
        let ((line, col), _) = lexer.line_col(span);
        let name = lexer.span_str(span).to_string();
        Self { line, col, name }
    }
}

#[derive(Debug)]
pub struct ArrVar {
    pub line: usize,
    pub col: usize,
    pub name: String,
    pub dims: Vec<u32>,
}

impl ArrVar {
    pub fn new(
        span: Span,
        dims: Vec<u32>,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Self {
        let ((line, col), _) = lexer.line_col(span);
        let name = lexer.span_str(span).to_string();
        Self {
            line,
            col,
            name,
            dims,
        }
    }
}

#[derive(Debug)]
pub enum UntypedVar {
    UniVar(UniVar),
    ArrVar(ArrVar),
}

pub fn int_from_span(
    span: Span,
    lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
) -> Result<u32, ParsingError> {
    let value = lexer.span_str(span).parse()?;
    Ok(value)
}
