use std::collections::HashMap;

use cfgrammar::Span;
use lrlex::DefaultLexerTypes;
use lrpar::NonStreamingLexer;

use crate::{ast::ASTNode, errors::ParsingError, get_symbol};

#[derive(Debug, Clone)]
pub enum SymbolEntry {
    LitInt(SymbolLitInt),
    LitFloat(SymbolLitFloat),
    LitChar(SymbolLitChar),
    LitBool(SymbolLitBool),
    Var(CommonAttrs),
    Arr(SymbolArr),
    Fn(SymbolFn),
    None,
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
            SymbolEntry::None => "NONE",
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            SymbolEntry::Var(symbol) => symbol.ty.clone(),
            SymbolEntry::Arr(symbol) => symbol.common.ty.clone(),
            SymbolEntry::Fn(symbol) => symbol.common.ty.clone(),
            SymbolEntry::LitInt(_) => Type::INT,
            SymbolEntry::LitFloat(_) => Type::FLOAT,
            SymbolEntry::LitChar(_) => Type::CHAR,
            SymbolEntry::LitBool(_) => Type::BOOL,
            SymbolEntry::None => Type::UNKNOWN,
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
            SymbolEntry::None => (0, 0),
        }
    }

    pub fn from_untyped_var(var: UntypedVar, ty: Type) -> Self {
        SymbolEntry::Var(CommonAttrs {
            line: var.line,
            col: var.col,
            size: ty.get_size(),
            val: var.name,
            ty,
        })
    }

    pub fn from_untyped_global_declr(var: UntypedGlobalDeclr, ty: Type) -> Self {
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
                size: Type::INT.get_size(),
                val: lit,
            })
        } else if let Ok(lit) = str.parse::<f64>() {
            Self::LitFloat(SymbolLitFloat {
                line,
                col,
                size: Type::FLOAT.get_size(),
                val: lit,
                val_string: str.to_string(),
            })
        } else if let Ok(lit) = str.parse::<bool>() {
            Self::LitBool(SymbolLitBool {
                line,
                col,
                size: Type::BOOL.get_size(),
                val: lit,
            })
        } else {
            Self::LitChar(SymbolLitChar {
                line,
                col,
                size: Type::CHAR.get_size(),
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
    pub val_string: String,
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
    pub ty: Type,
}

impl CommonAttrs {
    pub fn new(
        name: String,
        ty: Type,
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
        ty: Type,
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
        ty: Type,
        span: Span,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
        args: Option<Vec<SymbolEntry>>,
    ) -> Self {
        let mut common = CommonAttrs::new(name, ty, span, lexer);
        common.size = 0;
        Self { common, args }
    }
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    CHAR,
    INT,
    FLOAT,
    BOOL,
    UNKNOWN,
}

impl ToString for Type {
    fn to_string(&self) -> String {
        match self {
            Type::CHAR => "char".to_owned(),
            Type::INT => "int".to_owned(),
            Type::FLOAT => "float".to_owned(),
            Type::BOOL => "bool".to_owned(),
            Type::UNKNOWN => "UNKNOWN".to_owned(),
        }
    }
}

impl Type {
    pub fn get_size(&self) -> u32 {
        match self {
            Type::CHAR => 1,
            Type::INT => 4,
            Type::FLOAT => 8,
            Type::BOOL => 1,
            Type::UNKNOWN => 0,
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
        {
            println!("Attempting to add symbol {symbol:#?}");
            println!("Before adding: {self:#?}");
        }

        let scope_table = self.0.last_mut().ok_or(ParsingError::NoScope)?;
        match &symbol {
            SymbolEntry::LitInt(content) => {
                let key = content.val.to_string();
                scope_table.add_symbol(key, symbol)?;
            }
            SymbolEntry::LitFloat(content) => {
                let key = content.val_string.clone();
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
                if content.common.ty == Type::CHAR {
                    return Err(ParsingError::ErrCharVector(format!(
                        "Attempting to declare a char array at line {}, col {}.",
                        content.common.line, content.common.col
                    )));
                }

                scope_table.add_symbol(content.common.val.clone(), symbol)?
            }
            SymbolEntry::Fn(content) => {
                scope_table.add_symbol(content.common.val.clone(), symbol)?
            }
            SymbolEntry::None => return Ok(()),
        }

        #[cfg(feature = "debug")]
        println!("After adding: {self:#?}");

        Ok(())
    }

    pub fn clear(&mut self) {
        self.0.clear();
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UsageType {
    Var,
    Arr,
    FnCall,
}

impl ToString for UsageType {
    fn to_string(&self) -> String {
        match self {
            UsageType::Var => "variable".to_owned(),
            UsageType::Arr => "array".to_owned(),
            UsageType::FnCall => "function".to_owned(),
        }
    }
}

pub fn try_coersion(
    type_1: Type,
    type_2: Type,
    span: Span,
    lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
) -> Result<Type, ParsingError> {
    let ((line, col), _) = lexer.line_col(span);
    #[cfg(feature = "debug")]
    println!(
        "Coersion try at line {}, col {} ===> type 1: {}. type 2: {}",
        line,
        col,
        type_1.to_string(),
        type_2.to_string()
    );

    if type_1 == type_2 {
        return Ok(type_1);
    }

    match type_1 {
        Type::CHAR => Err(ParsingError::ErrXToChar(format!(
            "Attempting to coerse type {} to char at line {}, col {}.",
            type_2.to_string(),
            line,
            col
        ))),
        Type::INT => match type_2 {
            Type::CHAR => Err(ParsingError::ErrCharToInt(format!(
                "Attempting to coerse type char to int at line {}, col {}.",
                line, col
            ))),
            Type::FLOAT => Ok(type_2),
            Type::BOOL => Ok(type_1),
            Type::UNKNOWN => Err(ParsingError::CoerseUnknown(
                "this should not happen!".to_string(),
            )),
            Type::INT => unreachable!(),
        },
        Type::FLOAT => match type_2 {
            Type::CHAR => Err(ParsingError::ErrCharToFloat(format!(
                "Attempting to coerse type char to float at line {}, col {}.",
                line, col
            ))),
            Type::INT => Ok(type_1),
            Type::BOOL => Ok(type_1),
            Type::UNKNOWN => Err(ParsingError::CoerseUnknown(
                "this should not happen!".to_string(),
            )),
            Type::FLOAT => unreachable!(),
        },
        Type::BOOL => match type_2 {
            Type::CHAR => Err(ParsingError::ErrCharToBool(format!(
                "Attempting to coerse type char to bool at line {}, col {}.",
                line, col
            ))),
            Type::INT => Ok(type_2),
            Type::FLOAT => Ok(type_2),
            Type::UNKNOWN => Err(ParsingError::CoerseUnknown(
                "this should not happen!".to_string(),
            )),
            Type::BOOL => unreachable!(),
        },
        Type::UNKNOWN => Err(ParsingError::CoerseUnknown(
            "this should not happen!".to_string(),
        )),
    }
}

pub fn check_declaration(
    ident: &ASTNode,
    lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    usage: UsageType,
) -> Result<SymbolEntry, ParsingError> {
    let symbol = get_symbol(ident.span()?, lexer)?;
    match symbol.clone() {
        SymbolEntry::Var(content) if usage != UsageType::Var => {
            let declr_line = content.line;
            let declr_col = content.col;
            let ((usage_line, usage_col), _) = lexer.line_col(ident.span()?);

            Err(ParsingError::ErrVariable(format!(
                "Atempting to use indent \"{}\" as {} at line {}, col {}. This identifier was declared as variable at line {}, col {}.",
                content.val,
                usage.to_string(),
                usage_line,
                usage_col,
                declr_line,
                declr_col
            )))
        }
        SymbolEntry::Arr(content) if usage != UsageType::Arr => {
            let declr_line = content.common.line;
            let declr_col = content.common.col;
            let ((usage_line, usage_col), _) = lexer.line_col(ident.span()?);

            Err(ParsingError::ErrArray(format!(
                "Atempting to use indent \"{}\" as {} at line {}, col {}. This identifier was declared as array at line {}, col {}.",
                content.common.val,
                usage.to_string(),
                usage_line,
                usage_col,
                declr_line,
                declr_col
            )))
        }
        SymbolEntry::Fn(content) if usage != UsageType::FnCall => {
            let declr_line = content.common.line;
            let declr_col = content.common.col;
            let ((usage_line, usage_col), _) = lexer.line_col(ident.span()?);

            Err(ParsingError::ErrFunction(format!(
                "Atempting to use indent \"{}\" as {} at line {}, col {}. This identifier was declared as function at line {}, col {}.",
                content.common.val,
                usage.to_string(),
                usage_line,
                usage_col,
                declr_line,
                declr_col
            )))
        }
        _ => Ok(symbol),
    }
}
