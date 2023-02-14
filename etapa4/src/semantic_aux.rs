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
    Var(SymbolVar),
    Arr(SymbolArr),
    Fn(SymbolFn),
}

impl SymbolEntry {
    pub fn get_line_col(&self) -> (usize, usize) {
        match self {
            SymbolEntry::LitInt(content) => (content.line, content.col),
            SymbolEntry::LitFloat(content) => (content.line, content.col),
            SymbolEntry::LitChar(content) => (content.line, content.col),
            SymbolEntry::LitBool(content) => (content.line, content.col),
            SymbolEntry::Var(content) => (content.line, content.col),
            SymbolEntry::Arr(content) => (content.line, content.col),
            SymbolEntry::Fn(content) => (content.line, content.col),
        }
    }

    pub fn from_untyped_var(var: UntypedVar, ty: SymbolType) -> Self {
        match var {
            UntypedVar::UniVar(var) => SymbolEntry::Var(SymbolVar {
                line: var.line,
                col: var.col,
                size: ty.get_size(),
                val: var.name,
                ty,
            }),
            UntypedVar::ArrVar(var) => SymbolEntry::Arr(SymbolArr {
                line: var.line,
                col: var.col,
                size: var.dims.iter().fold(ty.get_size(), |acc, val| acc * val),
                val: var.name,
                ty,
                dims: var.dims,
            }),
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolLitInt {
    pub line: usize,
    pub col: usize,
    pub size: usize,
    pub val: u32,
}

#[derive(Debug, Clone)]
pub struct SymbolLitFloat {
    pub line: usize,
    pub col: usize,
    pub size: usize,
    pub val: f64,
}

#[derive(Debug, Clone)]
pub struct SymbolLitChar {
    pub line: usize,
    pub col: usize,
    pub size: usize,
    pub val: char,
}

#[derive(Debug, Clone)]
pub struct SymbolLitBool {
    pub line: usize,
    pub col: usize,
    pub size: usize,
    pub val: bool,
}

#[derive(Debug, Clone)]
pub struct SymbolVar {
    pub line: usize,
    pub col: usize,
    pub size: usize,
    pub val: String,
    pub ty: SymbolType,
}

#[derive(Debug, Clone)]
pub struct SymbolArr {
    pub line: usize,
    pub col: usize,
    pub size: usize,
    pub val: String,
    pub ty: SymbolType,
    pub dims: Vec<usize>,
}

#[derive(Debug, Clone)]
pub struct SymbolFn {
    pub line: usize,
    pub col: usize,
    pub size: usize,
    pub val: String,
    pub ty: SymbolType,
    pub args: Option<Vec<Box<SymbolEntry>>>,
}

#[derive(Debug, Clone)]
pub enum SymbolType {
    CHAR,
    INT,
    FLOAT,
    BOOL,
}

impl SymbolType {
    pub fn get_size(&self) -> usize {
        match self {
            SymbolType::CHAR => 1usize,
            SymbolType::INT => 4usize,
            SymbolType::FLOAT => 8usize,
            SymbolType::BOOL => 1usize,
        }
    }
}

#[derive(Debug)]
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
                "variable with name \"{key}\" was first declared at line {}, col {}.",
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

#[derive(Debug)]
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
            "variable with name \"{key}\" was never declared"
        )))
    }

    pub fn new_scope(&mut self) {
        self.0.push(SymbolTable::new());
    }

    pub fn pop_scope(&mut self) {
        self.0.pop();
    }

    pub fn add_symbol(&mut self, symbol: SymbolEntry) -> Result<(), ParsingError> {
        let scope_table = self.0.last_mut().ok_or(ParsingError::NoScope)?;
        match &symbol {
            SymbolEntry::LitInt(content) => {
                let key = content.val.to_string();
                scope_table.add_symbol(key, symbol)?;
            }
            SymbolEntry::LitFloat(content) => {
                let key = content.val.to_string();
                scope_table.add_symbol(key, symbol)?;
            }
            SymbolEntry::LitChar(content) => {
                let key = content.val.to_string();
                scope_table.add_symbol(key, symbol)?;
            }
            SymbolEntry::LitBool(content) => {
                let key = content.val.to_string();
                scope_table.add_symbol(key, symbol)?;
            }
            SymbolEntry::Var(content) => scope_table.add_symbol(content.val.clone(), symbol)?,
            SymbolEntry::Arr(content) => scope_table.add_symbol(content.val.clone(), symbol)?,
            SymbolEntry::Fn(content) => scope_table.add_symbol(content.val.clone(), symbol)?,
        }

        Ok(())
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
    pub dims: Vec<usize>,
}

impl ArrVar {
    pub fn new(
        span: Span,
        dims: Vec<usize>,
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
) -> Result<usize, ParsingError> {
    let value = lexer.span_str(span).parse()?;
    Ok(value)
}
