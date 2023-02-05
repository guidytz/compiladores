use anyhow::bail;
use cfgrammar::Span;
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};

#[derive(Debug, PartialEq, Eq)]
pub enum ASTNode {
    FnDeclare(FnDeclare),
    LitInit(LitInit),
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
    pub fn add_next(self, next: Box<ASTNode>) -> Result<Self, anyhow::Error> {
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
            ASTNode::LitInit(mut node) => {
                node.add_next(next);
                ASTNode::LitInit(node)
            }
            ast_node => bail!("Error: {:#?} should not have a next child node.", ast_node),
        };
        Ok(ast_node)
    }

    pub fn print_label(&self, lexer: &LRNonStreamingLexer<DefaultLexerTypes>) {
        match self {
            ASTNode::FnDeclare(_) => unimplemented!(),
            ASTNode::LitInit(_) => unimplemented!(),
            ASTNode::CommAttrib(_) => unimplemented!(),
            ASTNode::CommFnCall(_) => unimplemented!(),
            ASTNode::CommReturn(_) => unimplemented!(),
            ASTNode::CommIf(_) => unimplemented!(),
            ASTNode::CommWhile(_) => unimplemented!(),
            ASTNode::ArrIdx(_) => unimplemented!(),
            ASTNode::ExprIdxNode(_) => unimplemented!(),
            ASTNode::ExprOr(_) => unimplemented!(),
            ASTNode::ExprAnd(_) => unimplemented!(),
            ASTNode::ExprEq(_) => unimplemented!(),
            ASTNode::ExprNeq(_) => unimplemented!(),
            ASTNode::ExprLt(_) => unimplemented!(),
            ASTNode::ExprGt(_) => unimplemented!(),
            ASTNode::ExprLe(_) => unimplemented!(),
            ASTNode::ExprGe(_) => unimplemented!(),
            ASTNode::ExprAdd(_) => unimplemented!(),
            ASTNode::ExprSub(_) => unimplemented!(),
            ASTNode::ExprMul(_) => unimplemented!(),
            ASTNode::ExprDiv(_) => unimplemented!(),
            ASTNode::ExprMod(_) => unimplemented!(),
            ASTNode::ExprNeg(_) => unimplemented!(),
            ASTNode::ExprInv(_) => unimplemented!(),
            ASTNode::LitInt(_) => unimplemented!(),
            ASTNode::LitFloat(_) => unimplemented!(),
            ASTNode::LitChar(_) => unimplemented!(),
            ASTNode::LitBool(_) => unimplemented!(),
            ASTNode::Identifier(_) => unimplemented!(),
            ASTNode::None => unimplemented!(),
        }
    }

    pub fn print(&self) {
        match self {
            ASTNode::FnDeclare(node) => node.print(),
            ASTNode::LitInit(_) => unimplemented!(),
            ASTNode::CommAttrib(_) => unimplemented!(),
            ASTNode::CommFnCall(_) => unimplemented!(),
            ASTNode::CommReturn(_) => unimplemented!(),
            ASTNode::CommIf(_) => unimplemented!(),
            ASTNode::CommWhile(_) => unimplemented!(),
            ASTNode::ArrIdx(_) => unimplemented!(),
            ASTNode::ExprIdxNode(_) => unimplemented!(),
            ASTNode::ExprOr(_) => unimplemented!(),
            ASTNode::ExprAnd(_) => unimplemented!(),
            ASTNode::ExprEq(_) => unimplemented!(),
            ASTNode::ExprNeq(_) => unimplemented!(),
            ASTNode::ExprLt(_) => unimplemented!(),
            ASTNode::ExprGt(_) => unimplemented!(),
            ASTNode::ExprLe(_) => unimplemented!(),
            ASTNode::ExprGe(_) => unimplemented!(),
            ASTNode::ExprAdd(_) => unimplemented!(),
            ASTNode::ExprSub(_) => unimplemented!(),
            ASTNode::ExprMul(_) => unimplemented!(),
            ASTNode::ExprDiv(_) => unimplemented!(),
            ASTNode::ExprMod(_) => unimplemented!(),
            ASTNode::ExprNeg(_) => unimplemented!(),
            ASTNode::ExprInv(_) => unimplemented!(),
            ASTNode::LitInt(_) => unimplemented!(),
            ASTNode::LitFloat(_) => unimplemented!(),
            ASTNode::LitChar(_) => unimplemented!(),
            ASTNode::LitBool(_) => unimplemented!(),
            ASTNode::Identifier(_) => unimplemented!(),
            ASTNode::None => unimplemented!(),
        }
    }

    pub fn span(&self) -> Result<Span, anyhow::Error> {
        match self {
            ASTNode::FnDeclare(node) => Ok(node.span),
            ASTNode::LitInit(node) => Ok(node.span),
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
            ASTNode::None => bail!("Empty node does not have a span"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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

    pub fn print(&self) {
        println!("{:p}", self);
        println!("{:p}, {:p}", self, self.comm);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LitInit {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub lit: Box<ASTNode>,
    pub next: Option<Box<ASTNode>>,
}

impl LitInit {
    pub fn new(span: Span, ident: Box<ASTNode>, lit: Box<ASTNode>) -> Self {
        Self {
            span,
            ident,
            lit,
            next: None,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CommAttrib {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub expr: Box<ASTNode>,
    pub next: Option<Box<ASTNode>>,
}

impl CommAttrib {
    pub fn new(span: Span, ident: Box<ASTNode>, expr: Box<ASTNode>) -> Self {
        Self {
            span,
            ident,
            expr,
            next: None,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CommFnCall {
    pub span: Span,
    pub expr: Option<Box<ASTNode>>,
    pub next: Option<Box<ASTNode>>,
}

impl CommFnCall {
    pub fn new(span: Span, expr: Option<Box<ASTNode>>) -> Self {
        Self {
            span,
            expr,
            next: None,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CommReturn {
    pub span: Span,
    pub expr: Box<ASTNode>,
}

impl CommReturn {
    pub fn new(span: Span, expr: Box<ASTNode>) -> Self {
        Self { span, expr }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CommIf {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub true_fst_comm: Box<ASTNode>,
    pub false_fst_comm: Option<Box<ASTNode>>,
    pub next: Option<Box<ASTNode>>,
}

impl CommIf {
    pub fn new(
        span: Span,
        expr: Box<ASTNode>,
        true_fst_comm: Box<ASTNode>,
        false_fst_comm: Option<Box<ASTNode>>,
    ) -> Self {
        Self {
            span,
            expr,
            true_fst_comm,
            false_fst_comm,
            next: None,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CommWhile {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub fst_comm: Box<ASTNode>,
    pub next: Option<Box<ASTNode>>,
}

impl CommWhile {
    pub fn new(span: Span, expr: Box<ASTNode>, fst_comm: Box<ASTNode>) -> Self {
        Self {
            span,
            expr,
            fst_comm,
            next: None,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArrIdx {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub expr_tree: Box<ASTNode>,
    pub next: Option<Box<ASTNode>>,
}

impl ArrIdx {
    pub fn new(span: Span, ident: Box<ASTNode>, expr_tree: Box<ASTNode>) -> Self {
        Self {
            span,
            ident,
            expr_tree,
            next: None,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExprIdxNode {
    pub span: Span,
    pub child_left: Option<Box<ASTNode>>,
    pub child_right: Box<ASTNode>,
}

impl ExprIdxNode {
    pub fn new(span: Span, child_left: Option<Box<ASTNode>>, child_right: Box<ASTNode>) -> Self {
        Self {
            span,
            child_left,
            child_right,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct BinOp {
    pub span: Span,
    pub child_left: Box<ASTNode>,
    pub child_right: Box<ASTNode>,
    pub next: Option<Box<ASTNode>>,
}

impl BinOp {
    pub fn new(span: Span, child_left: Box<ASTNode>, child_right: Box<ASTNode>) -> Self {
        Self {
            span,
            child_left,
            child_right,
            next: None,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnOp {
    pub span: Span,
    pub child: Box<ASTNode>,
    pub next: Option<Box<ASTNode>>,
}

impl UnOp {
    pub fn new(span: Span, child: Box<ASTNode>) -> Self {
        Self {
            span,
            child,
            next: None,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LitInt {
    pub span: Span,
    pub next: Option<Box<ASTNode>>,
}

impl LitInt {
    pub fn new(span: Span) -> Self {
        Self { span, next: None }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LitFloat {
    pub span: Span,
    pub next: Option<Box<ASTNode>>,
}

impl LitFloat {
    pub fn new(span: Span) -> Self {
        Self { span, next: None }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LitChar {
    pub span: Span,
    pub next: Option<Box<ASTNode>>,
}

impl LitChar {
    pub fn new(span: Span) -> Self {
        Self { span, next: None }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct LitBool {
    pub span: Span,
    pub next: Option<Box<ASTNode>>,
}

impl LitBool {
    pub fn new(span: Span) -> Self {
        Self { span, next: None }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier {
    pub span: Span,
    pub next: Option<Box<ASTNode>>,
}

impl Identifier {
    pub fn new(span: Span) -> Self {
        Self { span, next: None }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = Some(next);
    }
}
