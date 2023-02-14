use std::num::ParseIntError;

#[derive(Debug, thiserror::Error)]
pub enum ParsingError {
    #[error("Attempted use of undeclared variable: `{0}`")]
    ErrUndeclared(String),
    #[error("Attempting to declare an already declared variable: `{0}`")]
    ErrDeclared(String),
    #[error("`{0}`")]
    ErrVariable(String),
    #[error("`{0}`")]
    ErrArray(String),
    #[error("`{0}`")]
    ErrFunction(String),
    #[error("`{0}`")]
    ErrCharToInt(String),
    #[error("`{0}`")]
    ErrCharToFloat(String),
    #[error("`{0}`")]
    ErrCharToBool(String),
    #[error("`{0}`")]
    ErrCharVector(String),
    #[error("`{0}`")]
    ErrXToChar(String),

    #[error("Parse int error: `{0}`")]
    ParseIntError(#[from] ParseIntError),
    #[error("Span get error: `{0}`")]
    SpanError(String),
    #[error("Add next to None node error: `{0}`")]
    AddNextToNone(String),
}
// ERR_UNDECLARED 10 //2.2
// ERR_DECLARED 11 //2.2
// ERR_VARIABLE 20 //2.3
// ERR_ARRAY 21 //2.3
// ERR_FUNCTION 22 //2.3
// ERR_CHAR_TO_INT 31 //2.4
// ERR_CHAR_TO_FLOAT 32 //2.4
// ERR_CHAR_TO_BOOL 33 //2.4
// ERR_CHAR_VECTOR 34 //2.4
// ERR_X_TO_CHAR 35 //2.4
