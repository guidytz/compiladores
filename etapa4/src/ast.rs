use cfgrammar::Span;
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use lrpar::NonStreamingLexer;

use crate::{
    errors::ParsingError,
    get_symbol,
    semantic_aux::{try_coersion, Type},
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ASTNode {
    FnDeclare(FnDeclare),
    VarInit(VarInit),
    CommAttrib(CommAttrib),
    CommFnCall(CommFnCall),
    CommReturn(CommReturn),
    CommIf(CommIf),
    CommWhile(CommWhile),
    ArrIdx(ArrIdx),
    ExprIdxNode(ExprIdxNode),
    ExprOr(BinOp),
    ExprAnd(BinOp),
    ExprEq(BinOp),
    ExprNeq(BinOp),
    ExprLt(BinOp),
    ExprGt(BinOp),
    ExprLe(BinOp),
    ExprGe(BinOp),
    ExprAdd(BinOp),
    ExprSub(BinOp),
    ExprMul(BinOp),
    ExprDiv(BinOp),
    ExprMod(BinOp),
    ExprNeg(UnOp),
    ExprInv(UnOp),
    LitInt(LitInt),
    LitFloat(LitFloat),
    LitChar(LitChar),
    LitBool(LitBool),
    Identifier(Identifier),
    None,
}

impl ASTNode {
    pub fn add_next(self, next: Box<ASTNode>) -> Result<Self, ParsingError> {
        let ast_node = match self {
            ASTNode::CommAttrib(mut node) => {
                node.add_next(next);
                ASTNode::CommAttrib(node)
            }
            ASTNode::CommFnCall(mut node) => {
                node.add_next(next);
                ASTNode::CommFnCall(node)
            }
            ASTNode::CommIf(mut node) => {
                node.add_next(next);
                ASTNode::CommIf(node)
            }
            ASTNode::CommWhile(mut node) => {
                node.add_next(next);
                ASTNode::CommWhile(node)
            }
            ASTNode::ArrIdx(mut node) => {
                node.add_next(next);
                ASTNode::ArrIdx(node)
            }
            ASTNode::ExprOr(mut node) => {
                node.add_next(next);
                ASTNode::ExprOr(node)
            }
            ASTNode::ExprAnd(mut node) => {
                node.add_next(next);
                ASTNode::ExprAnd(node)
            }
            ASTNode::ExprEq(mut node) => {
                node.add_next(next);
                ASTNode::ExprEq(node)
            }
            ASTNode::ExprNeq(mut node) => {
                node.add_next(next);
                ASTNode::ExprNeq(node)
            }
            ASTNode::ExprLt(mut node) => {
                node.add_next(next);
                ASTNode::ExprLt(node)
            }
            ASTNode::ExprGt(mut node) => {
                node.add_next(next);
                ASTNode::ExprGt(node)
            }
            ASTNode::ExprLe(mut node) => {
                node.add_next(next);
                ASTNode::ExprLe(node)
            }
            ASTNode::ExprGe(mut node) => {
                node.add_next(next);
                ASTNode::ExprGe(node)
            }
            ASTNode::ExprAdd(mut node) => {
                node.add_next(next);
                ASTNode::ExprAdd(node)
            }
            ASTNode::ExprSub(mut node) => {
                node.add_next(next);
                ASTNode::ExprSub(node)
            }
            ASTNode::ExprMul(mut node) => {
                node.add_next(next);
                ASTNode::ExprMul(node)
            }
            ASTNode::ExprDiv(mut node) => {
                node.add_next(next);
                ASTNode::ExprDiv(node)
            }
            ASTNode::ExprMod(mut node) => {
                node.add_next(next);
                ASTNode::ExprMod(node)
            }
            ASTNode::ExprNeg(mut node) => {
                node.add_next(next);
                ASTNode::ExprNeg(node)
            }
            ASTNode::ExprInv(mut node) => {
                node.add_next(next);
                ASTNode::ExprInv(node)
            }
            ASTNode::LitInt(mut node) => {
                node.add_next(next);
                ASTNode::LitInt(node)
            }
            ASTNode::LitFloat(mut node) => {
                node.add_next(next);
                ASTNode::LitFloat(node)
            }
            ASTNode::LitChar(mut node) => {
                node.add_next(next);
                ASTNode::LitChar(node)
            }
            ASTNode::LitBool(mut node) => {
                node.add_next(next);
                ASTNode::LitBool(node)
            }
            ASTNode::Identifier(mut node) => {
                node.add_next(next);
                ASTNode::Identifier(node)
            }
            ASTNode::VarInit(mut node) => {
                node.add_next(next)?;
                ASTNode::VarInit(node)
            }
            ast_node => Err(ParsingError::AddNextToNone(format!(
                "{:#?} should not have a next child node.",
                ast_node
            )))?,
        };
        Ok(ast_node)
    }

    pub fn label_string(&self, lexer: &LRNonStreamingLexer<DefaultLexerTypes>) -> String {
        if *self == ASTNode::None {
            // Empty node has no label as it is not essentially a real node
            return "".to_string();
        }

        format!("{}\n", self.label(lexer))
    }

    pub fn to_string(&self, lexer: &LRNonStreamingLexer<DefaultLexerTypes>) -> String {
        let mut str = self.label_string(lexer);
        match self {
            ASTNode::FnDeclare(node) => {
                str += &node.comm.parent_string(&self);
                str += &node.next_fn.parent_string(&self);

                str += &node.comm.to_string(lexer);
                str += &node.next_fn.to_string(lexer);
            }
            ASTNode::VarInit(node) => {
                str += &node.ident.parent_string(&self);
                str += &node.lit.parent_string(&self);
                if let Some(next) = &node.next {
                    str += &next.parent_string(&self);
                }

                str += &node.ident.to_string(lexer);
                str += &node.lit.to_string(lexer);
                if let Some(next) = &node.next {
                    str += &next.to_string(lexer);
                }
            }
            ASTNode::CommAttrib(node) => {
                str += &node.ident.parent_string(&self);
                str += &node.expr.parent_string(&self);
                str += &node.next.parent_string(&self);

                str += &node.ident.to_string(lexer);
                str += &node.expr.to_string(lexer);
                str += &node.next.to_string(lexer);
            }
            ASTNode::CommFnCall(node) => {
                str += &node.expr.parent_string(&self);
                str += &node.next.parent_string(&self);

                str += &node.expr.to_string(lexer);
                str += &node.next.to_string(lexer);
            }
            ASTNode::CommReturn(node) => {
                str += &node.expr.parent_string(&self);

                str += &node.expr.to_string(lexer);
            }
            ASTNode::CommIf(node) => {
                str += &node.expr.parent_string(&self);
                str += &node.true_fst_comm.parent_string(&self);
                str += &node.false_fst_comm.parent_string(&self);
                str += &node.next.parent_string(&self);

                str += &node.expr.to_string(lexer);
                str += &node.true_fst_comm.to_string(lexer);
                str += &node.false_fst_comm.to_string(lexer);
                str += &node.next.to_string(lexer);
            }
            ASTNode::CommWhile(node) => {
                str += &node.expr.parent_string(&self);
                str += &node.fst_comm.parent_string(&self);
                str += &node.next.parent_string(&self);

                str += &node.expr.to_string(lexer);
                str += &node.fst_comm.to_string(lexer);
                str += &node.next.to_string(lexer);
            }
            ASTNode::ArrIdx(node) => {
                str += &node.ident.parent_string(&self);
                str += &node.expr_tree.parent_string(&self);
                str += &node.next.parent_string(&self);

                str += &node.ident.to_string(lexer);
                str += &node.expr_tree.to_string(lexer);
                str += &node.next.to_string(lexer);
            }
            ASTNode::ExprIdxNode(node) => {
                str += &node.child_left.parent_string(&self);
                str += &node.child_right.parent_string(&self);

                str += &node.child_left.to_string(lexer);
                str += &node.child_right.to_string(lexer);
            }
            ASTNode::ExprOr(node)
            | ASTNode::ExprAnd(node)
            | ASTNode::ExprEq(node)
            | ASTNode::ExprNeq(node)
            | ASTNode::ExprLt(node)
            | ASTNode::ExprGt(node)
            | ASTNode::ExprLe(node)
            | ASTNode::ExprGe(node)
            | ASTNode::ExprAdd(node)
            | ASTNode::ExprSub(node)
            | ASTNode::ExprMul(node)
            | ASTNode::ExprDiv(node)
            | ASTNode::ExprMod(node) => {
                str += &node.child_left.parent_string(&self);
                str += &node.child_right.parent_string(&self);
                str += &node.next.parent_string(&self);

                str += &node.child_left.to_string(lexer);
                str += &node.child_right.to_string(lexer);
                str += &node.next.to_string(lexer);
            }
            ASTNode::ExprNeg(node) | ASTNode::ExprInv(node) => {
                str += &node.child.parent_string(&self);
                str += &node.next.parent_string(&self);

                str += &node.child.to_string(lexer);
                str += &node.next.to_string(lexer);
            }
            ASTNode::LitInt(node) => {
                str += &node.next.parent_string(&self);
                str += &node.next.to_string(lexer);
            }
            ASTNode::LitFloat(node) => {
                str += &node.next.parent_string(&self);
                str += &node.next.to_string(lexer);
            }
            ASTNode::LitChar(node) => {
                str += &node.next.parent_string(&self);
                str += &node.next.to_string(lexer);
            }
            ASTNode::LitBool(node) => {
                str += &node.next.parent_string(&self);
                str += &node.next.to_string(lexer);
            }
            ASTNode::Identifier(node) => {
                str += &node.next.parent_string(&self);
                str += &node.next.to_string(lexer);
            }
            ASTNode::None => { /* NOT A REAL NODE */ }
        }
        str
    }

    fn parent_string(&self, parent: &ASTNode) -> String {
        if *self == ASTNode::None {
            // Empty node has no parent as it is not essentially a real node
            return "".to_string();
        }
        format!("{}, {}\n", parent.addr(), self.addr())
    }

    fn addr(&self) -> String {
        match self {
            ASTNode::FnDeclare(node) => format!("{node:p}"),
            ASTNode::VarInit(node) => format!("{node:p}"),
            ASTNode::CommAttrib(node) => format!("{node:p}"),
            ASTNode::CommFnCall(node) => format!("{node:p}"),
            ASTNode::CommReturn(node) => format!("{node:p}"),
            ASTNode::CommIf(node) => format!("{node:p}"),
            ASTNode::CommWhile(node) => format!("{node:p}"),
            ASTNode::ArrIdx(node) => format!("{node:p}"),
            ASTNode::ExprIdxNode(node) => format!("{node:p}"),
            ASTNode::ExprOr(node) => format!("{node:p}"),
            ASTNode::ExprAnd(node) => format!("{node:p}"),
            ASTNode::ExprEq(node) => format!("{node:p}"),
            ASTNode::ExprNeq(node) => format!("{node:p}"),
            ASTNode::ExprLt(node) => format!("{node:p}"),
            ASTNode::ExprGt(node) => format!("{node:p}"),
            ASTNode::ExprLe(node) => format!("{node:p}"),
            ASTNode::ExprGe(node) => format!("{node:p}"),
            ASTNode::ExprAdd(node) => format!("{node:p}"),
            ASTNode::ExprSub(node) => format!("{node:p}"),
            ASTNode::ExprMul(node) => format!("{node:p}"),
            ASTNode::ExprDiv(node) => format!("{node:p}"),
            ASTNode::ExprMod(node) => format!("{node:p}"),
            ASTNode::ExprNeg(node) => format!("{node:p}"),
            ASTNode::ExprInv(node) => format!("{node:p}"),
            ASTNode::LitInt(node) => format!("{node:p}"),
            ASTNode::LitFloat(node) => format!("{node:p}"),
            ASTNode::LitChar(node) => format!("{node:p}"),
            ASTNode::LitBool(node) => format!("{node:p}"),
            ASTNode::Identifier(node) => format!("{node:p}"),
            ASTNode::None => "".to_owned(),
        }
    }

    fn label(&self, lexer: &LRNonStreamingLexer<DefaultLexerTypes>) -> String {
        let label = match self {
            ASTNode::FnDeclare(node) => lexer.span_str(node.name).to_owned(),
            ASTNode::VarInit(_) => "<=".to_owned(),
            ASTNode::CommAttrib(_) => "=".to_owned(),
            ASTNode::CommFnCall(node) => {
                format!(
                    "call {}",
                    lexer.span_str(node.name.span().expect("Could not get function call name"))
                )
            }
            ASTNode::CommReturn(_) => "return".to_owned(),
            ASTNode::CommIf(_) => "if".to_owned(),
            ASTNode::CommWhile(_) => "while".to_owned(),
            ASTNode::ArrIdx(_) => "[]".to_owned(),
            ASTNode::ExprIdxNode(_) => "^".to_owned(),
            ASTNode::ExprOr(_) => "||".to_owned(),
            ASTNode::ExprAnd(_) => "&&".to_owned(),
            ASTNode::ExprEq(_) => "==".to_owned(),
            ASTNode::ExprNeq(_) => "!=".to_owned(),
            ASTNode::ExprLt(_) => "<".to_owned(),
            ASTNode::ExprGt(_) => ">".to_owned(),
            ASTNode::ExprLe(_) => "<=".to_owned(),
            ASTNode::ExprGe(_) => ">=".to_owned(),
            ASTNode::ExprAdd(_) => "+".to_owned(),
            ASTNode::ExprSub(_) => "-".to_owned(),
            ASTNode::ExprMul(_) => "*".to_owned(),
            ASTNode::ExprDiv(_) => "/".to_owned(),
            ASTNode::ExprMod(_) => "%".to_owned(),
            ASTNode::ExprNeg(_) => "!".to_owned(),
            ASTNode::ExprInv(_) => "-".to_owned(),
            ASTNode::LitInt(node) => lexer.span_str(node.span).to_owned(),
            ASTNode::LitFloat(node) => lexer.span_str(node.span).to_owned(),
            ASTNode::LitChar(node) => {
                let label = lexer.span_str(node.span).to_owned();
                label[1..label.len() - 1].to_owned()
            }
            ASTNode::LitBool(node) => lexer.span_str(node.span).to_owned(),
            ASTNode::Identifier(node) => lexer.span_str(node.span).to_owned(),
            ASTNode::None => return "".to_owned(),
        };
        format!("{} [label=\"{}\"];", self.addr(), label)
    }

    pub fn span(&self) -> Result<Span, ParsingError> {
        match self {
            ASTNode::FnDeclare(node) => Ok(node.span),
            ASTNode::VarInit(node) => Ok(node.span),
            ASTNode::CommAttrib(node) => Ok(node.span),
            ASTNode::CommFnCall(node) => Ok(node.span),
            ASTNode::CommReturn(node) => Ok(node.span),
            ASTNode::CommIf(node) => Ok(node.span),
            ASTNode::CommWhile(node) => Ok(node.span),
            ASTNode::ArrIdx(node) => Ok(node.span),
            ASTNode::ExprIdxNode(node) => Ok(node.span),
            ASTNode::ExprOr(node) => Ok(node.span),
            ASTNode::ExprAnd(node) => Ok(node.span),
            ASTNode::ExprEq(node) => Ok(node.span),
            ASTNode::ExprNeq(node) => Ok(node.span),
            ASTNode::ExprLt(node) => Ok(node.span),
            ASTNode::ExprGt(node) => Ok(node.span),
            ASTNode::ExprLe(node) => Ok(node.span),
            ASTNode::ExprGe(node) => Ok(node.span),
            ASTNode::ExprAdd(node) => Ok(node.span),
            ASTNode::ExprSub(node) => Ok(node.span),
            ASTNode::ExprMul(node) => Ok(node.span),
            ASTNode::ExprDiv(node) => Ok(node.span),
            ASTNode::ExprMod(node) => Ok(node.span),
            ASTNode::ExprNeg(node) => Ok(node.span),
            ASTNode::ExprInv(node) => Ok(node.span),
            ASTNode::LitInt(node) => Ok(node.span),
            ASTNode::LitFloat(node) => Ok(node.span),
            ASTNode::LitChar(node) => Ok(node.span),
            ASTNode::LitBool(node) => Ok(node.span),
            ASTNode::Identifier(node) => Ok(node.span),
            ASTNode::None => Err(ParsingError::SpanError(
                "Empty node does not have a span".to_string(),
            )),
        }
    }

    fn get_type(&self) -> Type {
        match self {
            ASTNode::VarInit(node) => node.ty.clone(),
            ASTNode::CommAttrib(node) => node.ty.clone(),
            ASTNode::CommFnCall(node) => node.ty.clone(),
            ASTNode::CommReturn(node) => node.ty.clone(),
            ASTNode::CommIf(node) => node.ty.clone(),
            ASTNode::CommWhile(node) => node.ty.clone(),
            ASTNode::ArrIdx(node) => node.ty.clone(),
            ASTNode::ExprIdxNode(node) => node.ty.clone(),
            ASTNode::ExprOr(node) => node.ty.clone(),
            ASTNode::ExprAnd(node) => node.ty.clone(),
            ASTNode::ExprEq(node) => node.ty.clone(),
            ASTNode::ExprNeq(node) => node.ty.clone(),
            ASTNode::ExprLt(node) => node.ty.clone(),
            ASTNode::ExprGt(node) => node.ty.clone(),
            ASTNode::ExprLe(node) => node.ty.clone(),
            ASTNode::ExprGe(node) => node.ty.clone(),
            ASTNode::ExprAdd(node) => node.ty.clone(),
            ASTNode::ExprSub(node) => node.ty.clone(),
            ASTNode::ExprMul(node) => node.ty.clone(),
            ASTNode::ExprDiv(node) => node.ty.clone(),
            ASTNode::ExprMod(node) => node.ty.clone(),
            ASTNode::ExprNeg(node) => node.ty.clone(),
            ASTNode::ExprInv(node) => node.ty.clone(),
            ASTNode::Identifier(node) => node.ty.clone(),
            ASTNode::LitInt(_) => Type::INT,
            ASTNode::LitFloat(_) => Type::FLOAT,
            ASTNode::LitChar(_) => Type::CHAR,
            ASTNode::LitBool(_) => Type::BOOL,
            ASTNode::FnDeclare(_) => Type::UNKNOWN,
            ASTNode::None => Type::UNKNOWN,
        }
    }

    pub fn update_type(
        self,
        ty: Type,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        match self {
            ASTNode::VarInit(node) => {
                let node = node.update_type(ty, lexer)?;
                Ok(ASTNode::VarInit(node))
            }
            _ => Ok(self), // other nodes don't update types
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnDeclare {
    pub span: Span,
    pub comm: Box<ASTNode>,
    pub next_fn: Box<ASTNode>,
    pub name: Span,
}

impl FnDeclare {
    pub fn new(span: Span, comm: Box<ASTNode>, name: Span) -> Self {
        Self {
            span,
            comm,
            next_fn: Box::new(ASTNode::None),
            name,
        }
    }

    pub fn add_next_fn(&mut self, next_fn: Box<ASTNode>) {
        self.next_fn = next_fn;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarInit {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub lit: Box<ASTNode>,
    pub next: Option<Box<ASTNode>>,
    pub ty: Type,
}

impl VarInit {
    pub fn new(
        span: Span,
        ident: Box<ASTNode>,
        lit: Box<ASTNode>,
        next: Option<Box<ASTNode>>,
    ) -> Self {
        // identifier was not added to the symbol table at this point, type inference must be done later
        let ty = Type::UNKNOWN;
        Self {
            span,
            ident,
            lit,
            next,
            ty,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) -> Result<(), ParsingError> {
        match &self.next {
            Some(node) => {
                self.next = match **node {
                    ASTNode::None => Some(next),
                    _ => Some(Box::new(node.clone().add_next(next)?)),
                }
            }
            None => self.next = Some(next),
        }
        Ok(())
    }

    pub fn update_type(
        self,
        ty: Type,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        let mut node = self.clone();
        try_coersion(ty.clone(), node.lit.get_type(), node.span, lexer)?;
        node.ty = ty.clone();
        if let Some(next) = self.next.clone() {
            let next = next.update_type(ty, lexer)?;
            node.next = Some(Box::new(next));
        }
        Ok(node)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommAttrib {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub expr: Box<ASTNode>,
    pub next: Box<ASTNode>,
    ty: Type,
}

impl CommAttrib {
    pub fn new(
        span: Span,
        ident: Box<ASTNode>,
        expr: Box<ASTNode>,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        try_coersion(ident.get_type(), expr.get_type(), span, lexer)?;
        let ty = ident.get_type();
        Ok(Self {
            span,
            ident,
            expr,
            next: Box::new(ASTNode::None),
            ty,
        })
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommFnCall {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub name: Box<ASTNode>,
    pub ty: Type,
}

impl CommFnCall {
    pub fn new(span: Span, expr: Box<ASTNode>, ident: Box<ASTNode>) -> Self {
        let ty = ident.get_type();
        Self {
            span,
            expr,
            name: ident,
            next: Box::new(ASTNode::None),
            ty,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommReturn {
    pub span: Span,
    pub expr: Box<ASTNode>,
    ty: Type,
}

impl CommReturn {
    pub fn new(span: Span, expr: Box<ASTNode>) -> Self {
        let ty = expr.get_type();
        Self { span, expr, ty }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommIf {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub true_fst_comm: Box<ASTNode>,
    pub false_fst_comm: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub ty: Type,
}

impl CommIf {
    pub fn new(
        span: Span,
        expr: Box<ASTNode>,
        true_fst_comm: Box<ASTNode>,
        false_fst_comm: Box<ASTNode>,
    ) -> Self {
        let ty = expr.get_type();
        Self {
            span,
            expr,
            true_fst_comm,
            false_fst_comm,
            next: Box::new(ASTNode::None),
            ty,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommWhile {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub fst_comm: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub ty: Type,
}

impl CommWhile {
    pub fn new(span: Span, expr: Box<ASTNode>, fst_comm: Box<ASTNode>) -> Self {
        let ty = expr.get_type();
        Self {
            span,
            expr,
            fst_comm,
            next: Box::new(ASTNode::None),
            ty,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArrIdx {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub expr_tree: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub ty: Type,
}

impl ArrIdx {
    pub fn new(span: Span, ident: Box<ASTNode>, expr_tree: Box<ASTNode>) -> Self {
        let ty = ident.get_type();
        Self {
            span,
            ident,
            expr_tree,
            next: Box::new(ASTNode::None),
            ty,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExprIdxNode {
    pub span: Span,
    pub child_left: Box<ASTNode>,
    pub child_right: Box<ASTNode>,
    pub ty: Type,
}

impl ExprIdxNode {
    pub fn new(
        span: Span,
        child_left: Box<ASTNode>,
        child_right: Box<ASTNode>,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        let ty = try_coersion(child_left.get_type(), child_right.get_type(), span, lexer)?;
        Ok(Self {
            span,
            child_left,
            child_right,
            ty,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BinOp {
    pub span: Span,
    pub child_left: Box<ASTNode>,
    pub child_right: Box<ASTNode>,
    pub next: Box<ASTNode>,
    ty: Type,
}

impl BinOp {
    pub fn new(
        span: Span,
        child_left: Box<ASTNode>,
        child_right: Box<ASTNode>,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        let ty = try_coersion(child_left.get_type(), child_right.get_type(), span, lexer)?;
        Ok(Self {
            span,
            child_left,
            child_right,
            next: Box::new(ASTNode::None),
            ty,
        })
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnOp {
    pub span: Span,
    pub child: Box<ASTNode>,
    pub next: Box<ASTNode>,
    ty: Type,
}

impl UnOp {
    pub fn new(span: Span, child: Box<ASTNode>) -> Self {
        let ty = child.get_type();
        Self {
            span,
            child,
            next: Box::new(ASTNode::None),
            ty,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LitInt {
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl LitInt {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LitFloat {
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl LitFloat {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LitChar {
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl LitChar {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LitBool {
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl LitBool {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub span: Span,
    pub next: Box<ASTNode>,
    ty: Type,
}

impl Identifier {
    pub fn new(span: Span, lexer: &dyn NonStreamingLexer<DefaultLexerTypes>) -> Self {
        /*
           Node might be created, but type might be unknown at this point as the identifier might appear in a list.
            Therefore it might not be found in the symbol table yet.
        */
        let ty = match get_symbol(span, lexer) {
            Ok(symbol) => symbol.get_type(),
            Err(_) => Type::UNKNOWN,
        };

        Self {
            span,
            next: Box::new(ASTNode::None),
            ty,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}
