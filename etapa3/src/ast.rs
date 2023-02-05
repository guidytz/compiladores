use anyhow::bail;
use cfgrammar::Span;
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use lrpar::NonStreamingLexer;

#[derive(Debug, PartialEq, Eq)]
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
            ASTNode::VarInit(mut node) => {
                node.add_next(next);
                ASTNode::VarInit(node)
            }
            ast_node => bail!("Error: {:#?} should not have a next child node.", ast_node),
        };
        Ok(ast_node)
    }

    pub fn print_label(&self, lexer: &LRNonStreamingLexer<DefaultLexerTypes>) {
        if *self == ASTNode::None {
            // Empty node has no label
            return;
        }

        println!("{} {}", self.addr(), self.label(lexer));

        match self {
            ASTNode::FnDeclare(node) => {
                node.comm.print_label(lexer);
                node.next_fn.print_label(lexer);
            }
            ASTNode::VarInit(node) => {
                node.ident.print_label(lexer);
                node.lit.print_label(lexer);
                node.next_init.print_label(lexer);

                node.next.print_label(lexer);
            }
            ASTNode::CommAttrib(node) => {
                node.ident.print_label(lexer);
                node.expr.print_label(lexer);

                node.next.print_label(lexer);
            }
            ASTNode::CommFnCall(node) => {
                node.expr.print_label(lexer);

                node.next.print_label(lexer);
            }
            ASTNode::CommReturn(node) => {
                node.expr.print_label(lexer);
            }
            ASTNode::CommIf(node) => {
                node.expr.print_label(lexer);
                node.true_fst_comm.print_label(lexer);
                node.false_fst_comm.print_label(lexer);

                node.next.print_label(lexer);
            }
            ASTNode::CommWhile(node) => {
                node.expr.print_label(lexer);
                node.fst_comm.print_label(lexer);

                node.next.print_label(lexer);
            }
            ASTNode::ArrIdx(node) => {
                node.ident.print_label(lexer);
                node.expr_tree.print_label(lexer);

                node.next.print_label(lexer);
            }
            ASTNode::ExprIdxNode(node) => {
                node.child_left.print_label(lexer);
                node.child_right.print_label(lexer);
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
                node.child_left.print_label(lexer);
                node.child_right.print_label(lexer);

                node.next.print_label(lexer);
            }
            ASTNode::ExprNeg(node) | ASTNode::ExprInv(node) => {
                node.child.print_label(lexer);

                node.next.print_label(lexer);
            }
            ASTNode::LitInt(node) => {
                node.next.print_label(lexer);
            }
            ASTNode::LitFloat(node) => {
                node.next.print_label(lexer);
            }
            ASTNode::LitChar(node) => {
                node.next.print_label(lexer);
            }
            ASTNode::LitBool(node) => {
                node.next.print_label(lexer);
            }
            ASTNode::Identifier(node) => {
                node.next.print_label(lexer);
            }
            ASTNode::None => { /* EMPTY NODE */ }
        }
    }

    pub fn print(&self) {
        match self {
            ASTNode::FnDeclare(node) => {
                node.comm.print_parent(&self);
                node.next_fn.print_parent(&self);

                node.comm.print();
                node.next_fn.print();
            }
            ASTNode::VarInit(node) => {
                node.ident.print_parent(&self);
                node.lit.print_parent(&self);
                node.next_init.print_parent(&self);

                node.next.print_parent(&self);

                node.next_init.print();
                node.next.print();
            }
            ASTNode::CommAttrib(node) => {
                node.ident.print_parent(&self);
                node.expr.print_parent(&self);

                node.ident.print();
                node.expr.print();
            }
            ASTNode::CommFnCall(node) => {
                node.expr.print_parent(&self);
                node.next.print_parent(&self);

                node.expr.print();
                node.next.print();
            }
            ASTNode::CommReturn(node) => node.expr.print_parent(&self),
            ASTNode::CommIf(node) => {
                node.expr.print_parent(&self);
                node.true_fst_comm.print_parent(&self);
                node.false_fst_comm.print_parent(&self);
                node.next.print_parent(&self);

                node.true_fst_comm.print();
                node.false_fst_comm.print();
                node.next.print();
            }
            ASTNode::CommWhile(node) => {
                node.expr.print_parent(&self);
                node.fst_comm.print_parent(&self);
                node.next.print_parent(&self);

                node.expr.print();
                node.fst_comm.print();
                node.next.print();
            }
            ASTNode::ArrIdx(node) => {
                node.ident.print_parent(&self);
                node.expr_tree.print_parent(&self);
                node.next.print_parent(&self);

                node.ident.print();
                node.expr_tree.print();
                node.next.print();
            }
            ASTNode::ExprIdxNode(node) => {
                node.child_left.print_parent(&self);
                node.child_right.print_parent(&self);

                node.child_left.print();
                node.child_right.print();
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
                node.child_left.print_parent(&self);
                node.child_right.print_parent(&self);
                node.next.print_parent(&self);

                node.child_left.print();
                node.child_right.print();
                node.next.print();
            }
            ASTNode::ExprNeg(node) | ASTNode::ExprInv(node) => {
                node.child.print_parent(&self);
                node.next.print_parent(&self);

                node.child.print();
                node.next.print();
            }
            ASTNode::LitInt(_)
            | ASTNode::LitFloat(_)
            | ASTNode::LitChar(_)
            | ASTNode::LitBool(_)
            | ASTNode::Identifier(_)
            | ASTNode::None => { /* LEAFS */ }
        }
    }

    fn print_parent(&self, parent: &ASTNode) {
        if *self == ASTNode::None {
            return;
        }
        println!("{}, {}", parent.addr(), self.addr());
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
            ASTNode::CommFnCall(node) => format!("call {}", lexer.span_str(node.name)),
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
        format!("[label=\"{}\"];", label)
    }

    pub fn span(&self) -> Result<Span, anyhow::Error> {
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
}

#[derive(Debug, PartialEq, Eq)]
pub struct VarInit {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub lit: Box<ASTNode>,
    pub next_init: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl VarInit {
    pub fn new(
        span: Span,
        ident: Box<ASTNode>,
        lit: Box<ASTNode>,
        next_init: Box<ASTNode>,
    ) -> Self {
        Self {
            span,
            ident,
            lit,
            next_init,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CommAttrib {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub expr: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl CommAttrib {
    pub fn new(span: Span, ident: Box<ASTNode>, expr: Box<ASTNode>) -> Self {
        Self {
            span,
            ident,
            expr,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CommFnCall {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub name: Span,
    pub next: Box<ASTNode>,
}

impl CommFnCall {
    pub fn new(span: Span, expr: Box<ASTNode>, name: Span) -> Self {
        Self {
            span,
            expr,
            name,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
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
    pub false_fst_comm: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl CommIf {
    pub fn new(
        span: Span,
        expr: Box<ASTNode>,
        true_fst_comm: Box<ASTNode>,
        false_fst_comm: Box<ASTNode>,
    ) -> Self {
        Self {
            span,
            expr,
            true_fst_comm,
            false_fst_comm,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CommWhile {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub fst_comm: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl CommWhile {
    pub fn new(span: Span, expr: Box<ASTNode>, fst_comm: Box<ASTNode>) -> Self {
        Self {
            span,
            expr,
            fst_comm,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ArrIdx {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub expr_tree: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl ArrIdx {
    pub fn new(span: Span, ident: Box<ASTNode>, expr_tree: Box<ASTNode>) -> Self {
        Self {
            span,
            ident,
            expr_tree,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ExprIdxNode {
    pub span: Span,
    pub child_left: Box<ASTNode>,
    pub child_right: Box<ASTNode>,
}

impl ExprIdxNode {
    pub fn new(span: Span, child_left: Box<ASTNode>, child_right: Box<ASTNode>) -> Self {
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
    pub next: Box<ASTNode>,
}

impl BinOp {
    pub fn new(span: Span, child_left: Box<ASTNode>, child_right: Box<ASTNode>) -> Self {
        Self {
            span,
            child_left,
            child_right,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnOp {
    pub span: Span,
    pub child: Box<ASTNode>,
    pub next: Box<ASTNode>,
}

impl UnOp {
    pub fn new(span: Span, child: Box<ASTNode>) -> Self {
        Self {
            span,
            child,
            next: Box::new(ASTNode::None),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next;
    }
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
pub struct Identifier {
    pub span: Span,
    pub next: Box<ASTNode>,
}

impl Identifier {
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
