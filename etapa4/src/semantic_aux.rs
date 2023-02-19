use std::collections::HashMap;

use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

use crate::{ast::ASTNode, errors::ParsingError, SCOPE_STACK};

#[derive(Debug, Clone)]
pub enum SymbolEntry {
    LitInt(SymbolLitInt),
    LitFloat(SymbolLitFloat),
    LitChar(SymbolLitChar),
    LitBool(SymbolLitBool),
    Var(CommonAttrs),
    Arr(SymbolArr),
    Fn(SymbolFn),
}

impl SymbolEntry {
    pub fn type_str(&self) -> &'static str {
        match self {
            SymbolEntry::LitInt(_) => "literal int",
            SymbolEntry::LitFloat(_) => "literal float",
            SymbolEntry::LitChar(_) => "literal char",
            SymbolEntry::LitBool(_) => "literal bool",
            SymbolEntry::Var(_) => "variable",
            SymbolEntry::Arr(_) => "array",
            SymbolEntry::Fn(_) => "function",
        }
    }

    pub fn get_line_col(&self) -> (usize, usize) {
        match self {
            SymbolEntry::LitInt(content) => (content.line, content.col),
            SymbolEntry::LitFloat(content) => (content.line, content.col),
            SymbolEntry::LitChar(content) => (content.line, content.col),
            SymbolEntry::LitBool(content) => (content.line, content.col),
            SymbolEntry::Var(content) => (content.line, content.col),
            SymbolEntry::Arr(content) => (content.common.line, content.common.col),
            SymbolEntry::Fn(content) => (content.common.line, content.common.col),
        }
    }

    pub fn from_untyped_var(var: UntypedVar, ty: SymbolType) -> Self {
        SymbolEntry::Var(CommonAttrs {
            line: var.line,
            col: var.col,
            size: ty.get_size(),
            val: var.name,
            ty,
        })
    }

    pub fn from_untyped_global_declr(var: UntypedGlobalDeclr, ty: SymbolType) -> Self {
        match var {
            UntypedGlobalDeclr::Var(var) => SymbolEntry::from_untyped_var(var, ty.clone()),
            UntypedGlobalDeclr::Arr(var) => SymbolEntry::Arr(SymbolArr {
                common: CommonAttrs {
                    line: var.line,
                    col: var.col,
                    size: var.dims.iter().fold(ty.get_size(), |acc, val| acc * val),
                    val: var.name,
                    ty,
                },
                dims: var.dims,
            }),
        }
    }

    pub fn is_literal(&self) -> bool {
        match self {
            SymbolEntry::Var(_) => false,
            SymbolEntry::Arr(_) => false,
            SymbolEntry::Fn(_) => false,
            _ => true,
        }
    }

    pub fn from_lit_span(span: Span, lexer: &dyn NonStreamingLexer<DefaultLexerTypes>) -> Self {
        let str = lexer.span_str(span);
        let ((line, col), _) = lexer.line_col(span);
        if let Ok(lit) = str.parse::<u32>() {
            Self::LitInt(SymbolLitInt {
                line,
                col,
                size: SymbolType::INT.get_size(),
                val: lit,
            })
        } else if let Ok(lit) = str.parse::<f64>() {
            Self::LitFloat(SymbolLitFloat {
                line,
                col,
                size: SymbolType::FLOAT.get_size(),
                val: lit,
            })
        } else if let Ok(lit) = str.parse::<bool>() {
            Self::LitBool(SymbolLitBool {
                line,
                col,
                size: SymbolType::BOOL.get_size(),
                val: lit,
            })
        } else {
            Self::LitChar(SymbolLitChar {
                line,
                col,
                size: SymbolType::CHAR.get_size(),
                val: str.to_owned(),
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolLitInt {
    pub line: usize,
    pub col: usize,
    pub size: u32,
    pub val: u32,
}

#[derive(Debug, Clone)]
pub struct SymbolLitFloat {
    pub line: usize,
    pub col: usize,
    pub size: u32,
    pub val: f64,
}

#[derive(Debug, Clone)]
pub struct SymbolLitChar {
    pub line: usize,
    pub col: usize,
    pub size: u32,
    pub val: String,
}

#[derive(Debug, Clone)]
pub struct SymbolLitBool {
    pub line: usize,
    pub col: usize,
    pub size: u32,
    pub val: bool,
}

#[derive(Debug, Clone)]
pub struct CommonAttrs {
    pub line: usize,
    pub col: usize,
    pub size: u32,
    pub val: String,
    pub ty: SymbolType,
}

impl CommonAttrs {
    pub fn new(
        name: String,
        ty: SymbolType,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Self {
        let ((line, col), _) = lexer.line_col(span);
        let val = name;
        let size = ty.get_size();
        Self {
            line,
            col,
            size,
            val,
            ty,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolArr {
    pub common: CommonAttrs,
    pub dims: Vec<u32>,
}

impl SymbolArr {
    pub fn new(
        name: String,
        ty: SymbolType,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
        dims: Vec<u32>,
    ) -> Self {
        let common = CommonAttrs::new(name, ty, span, lexer);
        Self { common, dims }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolFn {
    pub common: CommonAttrs,
    pub args: Option<Vec<SymbolEntry>>,
}

impl SymbolFn {
    pub fn new(
        name: String,
        ty: SymbolType,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
        args: Option<Vec<SymbolEntry>>,
    ) -> Self {
        let common = CommonAttrs::new(name, ty, span, lexer);
        Self { common, args }
    }
}
#[derive(Debug, Clone)]
pub enum SymbolType {
    CHAR,
    INT,
    FLOAT,
    BOOL,
}

impl SymbolType {
    pub fn get_size(&self) -> u32 {
        match self {
            SymbolType::CHAR => 1,
            SymbolType::INT => 4,
            SymbolType::FLOAT => 8,
            SymbolType::BOOL => 1,
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
        if let Some(declared) = self.0.get(&key) {
            if declared.is_literal() {
                return Ok(());
            }

            let (s_line, s_col) = symbol.get_line_col();
            let (line, col) = declared.get_line_col();
            return Err(ParsingError::ErrDeclared(format!(
                "{} at line {s_line}, col {s_col}: {} with identifier \"{key}\" was first declared at line {}, col {}.",
                symbol.type_str(), declared.type_str(), line, col
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

    pub fn get_symbol(
        &self,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<SymbolEntry, ParsingError> {
        let key = lexer.span_str(span).to_string();
        for table in self.0.iter().rev() {
            match table.get(&key) {
                Some(symbol) => return Ok(symbol.clone()),
                None => continue,
            }
        }
        let ((line, col), _) = lexer.line_col(span);

        Err(ParsingError::ErrUndeclared(format!(
            "Identifier with name \"{key}\" at line {line}, col {col} was never declared"
        )))
    }

    pub fn new_scope(&mut self) {
        self.0.push(SymbolTable::new());
    }

    pub fn pop_scope(&mut self) {
        self.0.pop();
    }

    pub fn add_symbol(&mut self, symbol: SymbolEntry) -> Result<(), ParsingError> {
        #[cfg(feature = "debug")]
        println!("Before adding: {self:#?}");

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
            SymbolEntry::Arr(content) => {
                scope_table.add_symbol(content.common.val.clone(), symbol)?
            }
            SymbolEntry::Fn(content) => {
                scope_table.add_symbol(content.common.val.clone(), symbol)?
            }
        }

        #[cfg(feature = "debug")]
        println!("After adding: {self:#?}");

        Ok(())
    }
}

#[derive(Debug)]
pub struct UntypedVar {
    pub line: usize,
    pub col: usize,
    pub name: String,
}

impl UntypedVar {
    pub fn new(span: Span, lexer: &dyn NonStreamingLexer<DefaultLexerTypes>) -> Self {
        let ((line, col), _) = lexer.line_col(span);
        let name = lexer.span_str(span).to_string();
        Self { line, col, name }
    }
}

#[derive(Debug)]
pub struct UntypedArr {
    pub line: usize,
    pub col: usize,
    pub name: String,
    pub dims: Vec<u32>,
}

impl UntypedArr {
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
pub enum UntypedGlobalDeclr {
    Var(UntypedVar),
    Arr(UntypedArr),
}

#[derive(Debug)]
pub struct LocalDeclrAux {
    pub vars: Vec<UntypedVar>,
    pub node: ASTNode,
}

impl LocalDeclrAux {
    pub fn new(vars: Vec<UntypedVar>, node: ASTNode) -> Self {
        Self { vars, node }
    }
    pub fn with_vars(vars: Vec<UntypedVar>) -> Self {
        Self {
            vars,
            node: ASTNode::None,
        }
    }
}

pub fn int_from_span(
    span: Span,
    lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
) -> Result<u32, ParsingError> {
    let value = lexer.span_str(span).parse()?;
    Ok(value)
}

pub fn float_from_span(
    span: Span,
    lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
) -> Result<f64, ParsingError> {
    let value = lexer.span_str(span).parse()?;
    Ok(value)
}

pub fn char_from_span(
    span: Span,
    lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
) -> Result<char, ParsingError> {
    let value = lexer.span_str(span).parse()?;
    Ok(value)
}

pub fn bool_from_span(
    span: Span,
    lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
) -> Result<bool, ParsingError> {
    let value = lexer.span_str(span).parse()?;
    Ok(value)
}

pub fn check_declaration(
    ident: &ASTNode,
    lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
) -> Result<(), ParsingError> {
    SCOPE_STACK.with(|stack| stack.borrow().get_symbol(ident.span()?, lexer))?;
    Ok(())
}
