use std::num::ParseIntError;

#[derive(Debug, thiserror::Error)]
pub enum ParsingError {
    #[error("Attempted use of undeclared variable: {0}")]
    ErrUndeclared(String),
    #[error("Attempting to declare an already declared variable {0}")]
    ErrDeclared(String),
    #[error("{0}")]
    ErrVariable(String),
    #[error("{0}")]
    ErrArray(String),
    #[error("{0}")]
    ErrFunction(String),
    #[error("{0}")]
    ErrCharToInt(String),
    #[error("{0}")]
    ErrCharToFloat(String),
    #[error("{0}")]
    ErrCharToBool(String),
    #[error("{0}")]
    ErrCharVector(String),
    #[error("{0}")]
    ErrXToChar(String),

    #[error("Parse int error: {0}")]
    ParseIntError(#[from] ParseIntError),
    #[error("Span get error: {0}")]
    SpanError(String),
    #[error("Add next to None node error: {0}")]
    AddNextToNone(String),
    #[error("No scope defined")]
    NoScope,
}

impl ParsingError {
    pub fn to_err_code(&self) -> u8 {
        match self {
            ParsingError::ErrUndeclared(_) => 10,
            ParsingError::ErrDeclared(_) => 11,
            ParsingError::ErrVariable(_) => 20,
            ParsingError::ErrArray(_) => 21,
            ParsingError::ErrFunction(_) => 22,
            ParsingError::ErrCharToInt(_) => 31,
            ParsingError::ErrCharToFloat(_) => 32,
            ParsingError::ErrCharToBool(_) => 33,
            ParsingError::ErrCharVector(_) => 34,
            ParsingError::ErrXToChar(_) => 35,
            _ => 1,
        }
    }
}
