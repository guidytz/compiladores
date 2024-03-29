use cfgrammar::Span;
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use lrpar::NonStreamingLexer;

use crate::{
    errors::ParsingError,
    get_new_temp, get_symbol,
    semantic_aux::{try_coersion, Type},
};

#[cfg(feature = "code")]
use crate::{
    get_fn_label, get_fn_size, get_new_label, get_reg, get_var_deslocs,
    iloc_aux::{
        save_rfp_rsp, CmpInst, FullOp, IlocInst, In2Out, InOut, Jump, RETVAL_ADDR, RET_ADDR,
        RFP_ADDR, RSP_ADDR,
    },
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
    ExprEq(CmpOp),
    ExprNeq(CmpOp),
    ExprLt(CmpOp),
    ExprGt(CmpOp),
    ExprLe(CmpOp),
    ExprGe(CmpOp),
    ExprOr(BinOp),
    ExprAnd(BinOp),
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
            ASTNode::ExprEq(node)
            | ASTNode::ExprNeq(node)
            | ASTNode::ExprLt(node)
            | ASTNode::ExprGt(node)
            | ASTNode::ExprLe(node)
            | ASTNode::ExprGe(node) => {
                str += &node.child_left.parent_string(&self);
                str += &node.child_right.parent_string(&self);
                str += &node.next.parent_string(&self);

                str += &node.child_left.to_string(lexer);
                str += &node.child_right.to_string(lexer);
                str += &node.next.to_string(lexer);
            }
            ASTNode::ExprOr(node)
            | ASTNode::ExprAnd(node)
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

    pub fn gen_self_code(
        self,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        match self {
            ASTNode::VarInit(mut node) => {
                node.gen_self_code(lexer)?;
                Ok(ASTNode::VarInit(node))
            }
            _ => Ok(self), // other nodes should already have generated code
        }
    }

    #[cfg(feature = "code")]
    pub fn code(&self) -> Vec<IlocInst> {
        match self {
            ASTNode::FnDeclare(node) => node.code.clone(),
            ASTNode::VarInit(node) => node.code.clone(),
            ASTNode::CommAttrib(node) => node.code.clone(),
            ASTNode::CommFnCall(node) => node.code.clone(),
            ASTNode::CommReturn(node) => node.code.clone(),
            ASTNode::CommIf(node) => node.code.clone(),
            ASTNode::CommWhile(node) => node.code.clone(),
            ASTNode::ArrIdx(node) => node.code.clone(),
            ASTNode::ExprIdxNode(node) => node.code.clone(),
            ASTNode::ExprEq(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Cmp(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_EQ".to_string();
                        }
                        IlocInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprNeq(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Cmp(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_NE".to_string();
                        }
                        IlocInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprLt(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Cmp(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_LT".to_string();
                        }
                        IlocInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprGt(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Cmp(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_GT".to_string();
                        }
                        IlocInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprLe(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Cmp(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_LE".to_string();
                        }
                        IlocInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprGe(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Cmp(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "cmp_GE".to_string();
                        }
                        IlocInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprOr(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Arithm(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "or".to_string();
                        }
                        IlocInst::Arithm(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprAnd(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Arithm(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "and".to_string();
                        }
                        IlocInst::Arithm(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprAdd(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Arithm(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "add".to_string();
                        }
                        IlocInst::Arithm(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprSub(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Arithm(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "sub".to_string();
                        }
                        IlocInst::Arithm(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprMul(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Arithm(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "mult".to_string();
                        }
                        IlocInst::Arithm(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprDiv(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    IlocInst::Arithm(mut inst) => {
                        if inst.name.is_empty() {
                            inst.name = "div".to_string();
                        }
                        IlocInst::Arithm(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprMod(node) => node.code.clone(),
            ASTNode::ExprNeg(node) => node.code.clone(),
            ASTNode::ExprInv(node) => node.code.clone(),
            ASTNode::LitInt(node) => node.code.clone(),
            ASTNode::LitFloat(node) => node.code.clone(),
            ASTNode::LitChar(node) => node.code.clone(),
            ASTNode::LitBool(node) => node.code.clone(),
            ASTNode::Identifier(node) => node.code.clone(),
            ASTNode::None => vec![IlocInst::Empty],
        }
    }

    #[cfg(feature = "code")]
    pub fn temp(&self) -> String {
        match self {
            ASTNode::CommFnCall(node) => node.temp.clone(),
            ASTNode::ExprOr(node) => node.temp.clone(),
            ASTNode::ExprAnd(node) => node.temp.clone(),
            ASTNode::ExprEq(node) => node.temp.clone(),
            ASTNode::ExprNeq(node) => node.temp.clone(),
            ASTNode::ExprLt(node) => node.temp.clone(),
            ASTNode::ExprGt(node) => node.temp.clone(),
            ASTNode::ExprLe(node) => node.temp.clone(),
            ASTNode::ExprGe(node) => node.temp.clone(),
            ASTNode::ExprAdd(node) => node.temp.clone(),
            ASTNode::ExprSub(node) => node.temp.clone(),
            ASTNode::ExprMul(node) => node.temp.clone(),
            ASTNode::ExprDiv(node) => node.temp.clone(),
            ASTNode::LitInt(node) => node.temp.clone(),
            ASTNode::Identifier(node) => node.temp.clone(),
            ASTNode::FnDeclare(_) => unimplemented!("temp not implemented!"),
            ASTNode::VarInit(_) => unimplemented!("temp not implemented!"),
            ASTNode::CommAttrib(_) => unimplemented!("temp not implemented!"),
            ASTNode::ArrIdx(_) => unimplemented!("temp not implemented!"),
            ASTNode::ExprIdxNode(_) => unimplemented!("temp not implemented!"),
            ASTNode::CommReturn(_) => unimplemented!("temp not implemented!"),
            ASTNode::CommIf(_) => unimplemented!("temp not implemented!"),
            ASTNode::CommWhile(_) => unimplemented!("temp not implemented!"),
            ASTNode::ExprMod(_) => unimplemented!("temp not implemented!"),
            ASTNode::ExprNeg(_) => unimplemented!("temp not implemented!"),
            ASTNode::ExprInv(_) => unimplemented!("temp not implemented!"),
            ASTNode::LitFloat(_) => unimplemented!("temp not implemented!"),
            ASTNode::LitChar(_) => unimplemented!("temp not implemented!"),
            ASTNode::LitBool(_) => unimplemented!("temp not implemented!"),
            ASTNode::None => unimplemented!("temp not implemented!"),
        }
    }

    pub fn is_var_init(&self) -> bool {
        match self {
            ASTNode::VarInit(_) => true,
            _ => false,
        }
    }

    pub fn gen_load(
        &mut self,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<(), ParsingError> {
        match self {
            ASTNode::Identifier(node) => node.gen_load(lexer),
            _ => Ok(()),
        }
    }

    pub fn gen_init_code(&mut self) -> Result<(), ParsingError> {
        #[cfg(feature = "code")]
        {
            let mut code = vec![];
            let load_rfp = IlocInst::LoadImed(InOut::new(
                "loadI".to_string(),
                "1024".to_string(),
                "rfp".to_string(),
            ));
            let load_rsp = IlocInst::LoadImed(InOut::new(
                "loadI".to_string(),
                "1024".to_string(),
                "rsp".to_string(),
            ));

            let temp_rpc = get_new_temp();
            let load_rpc = IlocInst::Arithm(FullOp::new(
                "addI".to_string(),
                "rpc".to_string(),
                "3".to_string(),
                temp_rpc.clone(),
            ));
            let ret_addr_save = IlocInst::StoreDesl(In2Out::new(
                "storeAI".to_string(),
                temp_rpc,
                "rsp".to_string(),
                RET_ADDR.to_string(),
            ));
            let main_label = get_fn_label("main".to_string())?;
            let jump_main = IlocInst::Jump(Jump::new("jumpI".to_string(), main_label));

            code.push(load_rfp);
            code.push(load_rsp);
            code.extend(save_rfp_rsp());
            code.push(load_rpc);
            code.push(ret_addr_save);
            code.push(jump_main);
            code.push(IlocInst::Halt);

            match self {
                ASTNode::FnDeclare(node) => {
                    code.extend(node.code.clone());
                    node.code = code;
                    Ok(())
                }
                _ => Err(ParsingError::NotRootFunction),
            }?;
        }
        Ok(())
    }

    #[cfg(feature = "code")]
    pub fn get_temps(&self) -> Vec<String> {
        match self {
            ASTNode::ExprEq(expr) => expr.get_temps(),
            ASTNode::ExprNeq(expr) => expr.get_temps(),
            ASTNode::ExprLt(expr) => expr.get_temps(),
            ASTNode::ExprGt(expr) => expr.get_temps(),
            ASTNode::ExprLe(expr) => expr.get_temps(),
            ASTNode::ExprGe(expr) => expr.get_temps(),
            ASTNode::ExprOr(expr) => expr.get_temps(),
            ASTNode::ExprAnd(expr) => expr.get_temps(),
            ASTNode::ExprAdd(expr) => expr.get_temps(),
            ASTNode::ExprSub(expr) => expr.get_temps(),
            ASTNode::ExprMul(expr) => expr.get_temps(),
            ASTNode::ExprDiv(expr) => expr.get_temps(),
            ASTNode::ExprMod(expr) => expr.get_temps(),
            ASTNode::LitInt(expr) => expr.get_temps(),
            ASTNode::Identifier(expr) => expr.get_temps(),
            _ => vec![],
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FnDeclare {
    pub span: Span,
    pub comm: Box<ASTNode>,
    pub next_fn: Box<ASTNode>,
    pub name: Span,
    #[cfg(feature = "code")]
    pub code: Vec<IlocInst>,
}

impl FnDeclare {
    pub fn new(
        span: Span,
        comm: Box<ASTNode>,
        name: Span,
        _lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        #[cfg(feature = "code")]
        let code = {
            let symbol = get_symbol(name, _lexer)?;
            let label = symbol.get_label();
            let fun_size = get_fn_size(symbol.get_key())?;

            let mut code = vec![IlocInst::Nop(Some(label))];
            let update_rfp = IlocInst::LoadImed(InOut::new(
                "i2i".to_string(),
                "rsp".to_string(),
                "rfp".to_string(),
            ));
            let update_rsp = IlocInst::Arithm(FullOp::new(
                "addI".to_string(),
                "rsp".to_string(),
                fun_size.to_string(),
                "rsp".to_string(),
            ));
            code.push(update_rfp);
            code.push(update_rsp);

            code.extend(comm.code());

            code.extend(return_to_caller_insts());
            code
        };

        Ok(Self {
            span,
            comm,
            next_fn: Box::new(ASTNode::None),
            name,
            #[cfg(feature = "code")]
            code,
        })
    }

    pub fn add_next_fn(&mut self, next_fn: Box<ASTNode>) {
        self.next_fn = next_fn.clone();
        #[cfg(feature = "code")]
        self.code.extend(next_fn.code());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct VarInit {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub lit: Box<ASTNode>,
    pub next: Option<Box<ASTNode>>,
    pub ty: Type,
    #[cfg(feature = "code")]
    pub code: Vec<IlocInst>,
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
            #[cfg(feature = "code")]
            code: vec![],
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) -> Result<(), ParsingError> {
        match &self.next {
            Some(node) => {
                self.next = match **node {
                    ASTNode::None => Some(next.clone()),
                    _ => Some(Box::new(node.clone().add_next(next.clone())?)),
                }
            }
            None => self.next = Some(next.clone()),
        }
        #[cfg(feature = "code")]
        self.code.extend(next.code());
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

    pub fn gen_self_code(
        &mut self,
        _lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<(), ParsingError> {
        #[cfg(feature = "code")]
        {
            if let Some(next) = self.next.clone() {
                let next = next.gen_self_code(_lexer)?;
                self.next = Some(Box::new(next));
            }

            let symbol = get_symbol(self.ident.span()?, _lexer)?;

            let reg = get_reg(&symbol);
            let inst = IlocInst::StoreDesl(In2Out::new(
                "storeAI".to_string(),
                self.lit.temp(),
                reg,
                symbol.desloc().to_string(),
            ));
            let mut code = vec![];
            code.extend(self.lit.code());
            code.push(inst);
            if let Some(next) = self.next.clone() {
                code.extend(next.code());
            }
            self.code = code;
        };
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommAttrib {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub expr: Box<ASTNode>,
    pub next: Box<ASTNode>,
    ty: Type,
    #[cfg(feature = "code")]
    pub code: Vec<IlocInst>,
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

        #[cfg(feature = "code")]
        let code = {
            let symbol = get_symbol(ident.span()?, lexer)?;
            let reg = get_reg(&symbol);
            let inst = IlocInst::StoreDesl(In2Out::new(
                "storeAI".to_string(),
                expr.temp(),
                reg,
                symbol.desloc().to_string(),
            ));
            let mut code = vec![];
            code.extend(expr.code());
            code.push(inst);
            code
        };

        Ok(Self {
            span,
            ident,
            expr,
            next: Box::new(ASTNode::None),
            ty,
            #[cfg(feature = "code")]
            code,
        })
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommFnCall {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub name: Box<ASTNode>,
    pub ty: Type,
    #[cfg(feature = "code")]
    pub code: Vec<IlocInst>,
    #[cfg(feature = "code")]
    pub temp: String,
}

impl CommFnCall {
    pub fn new(
        span: Span,
        expr: Box<ASTNode>,
        ident: Box<ASTNode>,
        _lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        let ty = ident.get_type();

        #[cfg(feature = "code")]
        let temp = get_new_temp();

        #[cfg(feature = "code")]
        let code = {
            let mut code = vec![];
            let temps = expr.get_temps();
            let name = _lexer.span_str(ident.span()?).to_string();
            let mut deslocs = get_var_deslocs(name.clone())?;
            deslocs.sort_by_key(|vals| vals.1);

            code.extend(expr.code());
            temps
                .into_iter()
                .rev()
                .zip(deslocs)
                .for_each(|(temp, (_key, desloc))| {
                    #[cfg(feature = "debug")]
                    println!("{temp} -> {_key}: {desloc}");

                    let load_temp_to_param = IlocInst::StoreDesl(In2Out::new(
                        "storeAI".to_string(),
                        temp,
                        "rsp".to_string(),
                        desloc.to_string(),
                    ));
                    code.push(load_temp_to_param);
                });

            let temp_rpc = get_new_temp();
            let load_rpc = IlocInst::Arithm(FullOp::new(
                "addI".to_string(),
                "rpc".to_string(),
                3.to_string(),
                temp_rpc.clone(),
            ));
            let ret_addr_save = IlocInst::StoreDesl(In2Out::new(
                "storeAI".to_string(),
                temp_rpc,
                "rsp".to_string(),
                RET_ADDR.to_string(),
            ));
            let fn_label = get_fn_label(name)?;
            let jump_fn = IlocInst::Jump(Jump::new("jumpI".to_string(), fn_label));

            code.extend(save_rfp_rsp());
            code.push(load_rpc);
            code.push(ret_addr_save);
            code.push(jump_fn);

            let retrieve_ret_val = IlocInst::LoadDesl(FullOp::new(
                "loadAI".to_string(),
                "rsp".to_string(),
                RETVAL_ADDR.to_string(),
                temp.clone(),
            ));
            code.push(retrieve_ret_val);

            code
        };
        Ok(Self {
            span,
            expr,
            name: ident,
            next: Box::new(ASTNode::None),
            ty,
            #[cfg(feature = "code")]
            code,
            #[cfg(feature = "code")]
            temp,
        })
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommReturn {
    pub span: Span,
    pub expr: Box<ASTNode>,
    ty: Type,
    #[cfg(feature = "code")]
    pub code: Vec<IlocInst>,
}

impl CommReturn {
    pub fn new(span: Span, expr: Box<ASTNode>) -> Self {
        let ty = expr.get_type();

        #[cfg(feature = "code")]
        let code = {
            let mut code = vec![];
            code.extend(expr.code());
            let exp_temp = expr.temp();
            let save_ret_value = IlocInst::StoreDesl(In2Out::new(
                "storeAI".to_string(),
                exp_temp,
                "rfp".to_string(),
                RETVAL_ADDR.to_string(),
            ));
            code.push(save_ret_value);
            code.extend(return_to_caller_insts());
            code
        };
        Self {
            span,
            expr,
            ty,
            #[cfg(feature = "code")]
            code,
        }
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
    #[cfg(feature = "code")]
    pub code: Vec<IlocInst>,
}

impl CommIf {
    pub fn new(
        span: Span,
        expr: Box<ASTNode>,
        true_fst_comm: Box<ASTNode>,
        false_fst_comm: Box<ASTNode>,
    ) -> Self {
        let ty = expr.get_type();
        #[cfg(feature = "code")]
        let code = {
            let label_true = get_new_label();
            let label_false = get_new_label();
            let label_later = get_new_label();

            let temp = get_new_temp();
            let op_temp = get_new_temp();
            let load_op =
                IlocInst::LoadImed(InOut::new("loadI".to_string(), 0.to_string(), temp.clone()));
            let cmp_ne = IlocInst::Cmp(CmpInst::new(
                "cmp_NE".to_string(),
                expr.temp(),
                temp,
                op_temp.clone(),
            ));
            let cbr = IlocInst::Cbr(In2Out::new(
                "cbr".to_string(),
                op_temp,
                label_true.clone(),
                label_false.clone(),
            ));
            let true_nop = IlocInst::Nop(Some(label_true));
            let jump_later = IlocInst::Jump(Jump::new("jumpI".to_string(), label_later.clone()));
            let false_nop = IlocInst::Nop(Some(label_false));
            let later_nop = IlocInst::Nop(Some(label_later));

            let mut code = vec![];
            code.extend(expr.code());
            code.push(load_op);
            code.push(cmp_ne);
            code.push(cbr);
            code.push(true_nop);
            code.extend(true_fst_comm.code());
            code.push(jump_later);
            code.push(false_nop);
            code.extend(false_fst_comm.code());
            code.push(later_nop);
            code
        };

        Self {
            span,
            expr,
            true_fst_comm,
            false_fst_comm,
            next: Box::new(ASTNode::None),
            ty,
            #[cfg(feature = "code")]
            code,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CommWhile {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub fst_comm: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub ty: Type,
    #[cfg(feature = "code")]
    pub code: Vec<IlocInst>,
}

impl CommWhile {
    pub fn new(span: Span, expr: Box<ASTNode>, fst_comm: Box<ASTNode>) -> Self {
        let ty = expr.get_type();
        #[cfg(feature = "code")]
        let code = {
            let label_expr = get_new_label();
            let label_true = get_new_label();
            let label_later = get_new_label();

            let temp = get_new_temp();
            let op_temp = get_new_temp();
            let nop_expr = IlocInst::Nop(Some(label_expr.clone()));
            let load_op =
                IlocInst::LoadImed(InOut::new("loadI".to_string(), 0.to_string(), temp.clone()));
            let cmp_ne = IlocInst::Cmp(CmpInst::new(
                "cmp_NE".to_string(),
                expr.temp(),
                temp,
                op_temp.clone(),
            ));
            let cbr = IlocInst::Cbr(In2Out::new(
                "cbr".to_string(),
                op_temp,
                label_true.clone(),
                label_later.clone(),
            ));
            let true_nop = IlocInst::Nop(Some(label_true));
            let jump_back = IlocInst::Jump(Jump::new("jumpI".to_string(), label_expr.clone()));
            let later_nop = IlocInst::Nop(Some(label_later));

            let mut code = vec![];
            code.push(nop_expr);
            code.extend(expr.code());
            code.push(load_op);
            code.push(cmp_ne);
            code.push(cbr);
            code.push(true_nop);
            code.extend(fst_comm.code());
            code.push(jump_back);
            code.push(later_nop);
            code
        };

        Self {
            span,
            expr,
            fst_comm,
            next: Box::new(ASTNode::None),
            ty,
            #[cfg(feature = "code")]
            code,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArrIdx {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub expr_tree: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub ty: Type,
    #[cfg(feature = "code")]
    pub code: Vec<IlocInst>,
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
            #[cfg(feature = "code")]
            code: vec![],
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExprIdxNode {
    pub span: Span,
    pub child_left: Box<ASTNode>,
    pub child_right: Box<ASTNode>,
    pub ty: Type,
    #[cfg(feature = "code")]
    pub code: Vec<IlocInst>,
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
            #[cfg(feature = "code")]
            code: vec![],
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
    #[cfg(feature = "code")]
    temp: String,
    #[cfg(feature = "code")]
    code: Vec<IlocInst>,
}

impl BinOp {
    pub fn new(
        span: Span,
        child_left: Box<ASTNode>,
        child_right: Box<ASTNode>,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        let ty = try_coersion(child_left.get_type(), child_right.get_type(), span, lexer)?;
        #[cfg(feature = "code")]
        let temp = get_new_temp();
        #[cfg(feature = "code")]
        let code = {
            let inst = IlocInst::Arithm(FullOp::new(
                "".to_string(),
                child_left.temp(),
                child_right.temp(),
                temp.clone(),
            ));
            let mut code = vec![];

            code.extend(child_left.code());
            code.extend(child_right.code());
            code.push(inst);
            code
        };

        Ok(Self {
            span,
            child_left,
            child_right,
            next: Box::new(ASTNode::None),
            ty,
            #[cfg(feature = "code")]
            temp,
            #[cfg(feature = "code")]
            code,
        })
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }

    #[cfg(feature = "code")]
    pub fn get_temps(&self) -> Vec<String> {
        let mut temps = vec![self.temp.clone()];
        temps.extend(self.next.get_temps());
        temps
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CmpOp {
    pub span: Span,
    pub child_left: Box<ASTNode>,
    pub child_right: Box<ASTNode>,
    pub next: Box<ASTNode>,
    ty: Type,
    #[cfg(feature = "code")]
    temp: String,
    #[cfg(feature = "code")]
    code: Vec<IlocInst>,
}

impl CmpOp {
    pub fn new(
        span: Span,
        child_left: Box<ASTNode>,
        child_right: Box<ASTNode>,
        lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        let ty = try_coersion(child_left.get_type(), child_right.get_type(), span, lexer)?;
        #[cfg(feature = "code")]
        let temp = get_new_temp();

        #[cfg(feature = "code")]
        let code = {
            let label_true = get_new_label();
            let label_false = get_new_label();
            let label_end = get_new_label();

            let cmp_inst = IlocInst::Cmp(CmpInst::new(
                "".to_string(),
                child_left.temp(),
                child_right.temp(),
                temp.clone(),
            ));

            let cbr_inst = IlocInst::Cbr(In2Out::new(
                "cbr".to_string(),
                temp.clone(),
                label_true.clone(),
                label_false.clone(),
            ));

            let load_true = IlocInst::LoadImed(InOut::new(
                "loadI".to_string(),
                "1".to_string(),
                temp.clone(),
            ))
            .add_label(label_true);

            let jump_later = IlocInst::Jump(Jump::new("jumpI".to_string(), label_end.clone()));

            let load_false = IlocInst::LoadImed(InOut::new(
                "loadI".to_string(),
                "0".to_string(),
                temp.clone(),
            ))
            .add_label(label_false);

            let nop_inst = IlocInst::Nop(Some(label_end));

            let mut code = vec![];

            code.extend(child_left.code());
            code.extend(child_right.code());
            code.push(cmp_inst);
            code.push(cbr_inst);
            code.push(load_true);
            code.push(jump_later);
            code.push(load_false);
            code.push(nop_inst);
            code
        };

        Ok(Self {
            span,
            child_left,
            child_right,
            next: Box::new(ASTNode::None),
            ty,
            #[cfg(feature = "code")]
            temp,
            #[cfg(feature = "code")]
            code,
        })
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }

    #[cfg(feature = "code")]
    pub fn get_temps(&self) -> Vec<String> {
        let mut temps = vec![self.temp.clone()];
        temps.extend(self.next.get_temps());
        temps
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct UnOp {
    pub span: Span,
    pub child: Box<ASTNode>,
    pub next: Box<ASTNode>,
    ty: Type,
    #[cfg(feature = "code")]
    code: Vec<IlocInst>,
    #[cfg(feature = "code")]
    temp: String,
}

impl UnOp {
    pub fn new(span: Span, child: Box<ASTNode>) -> Self {
        let ty = child.get_type();
        Self {
            span,
            child,
            next: Box::new(ASTNode::None),
            ty,
            #[cfg(feature = "code")]
            code: vec![],
            #[cfg(feature = "code")]
            temp: "".to_string(),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LitInt {
    pub span: Span,
    pub next: Box<ASTNode>,
    #[cfg(feature = "code")]
    code: Vec<IlocInst>,
    temp: String,
}

impl LitInt {
    pub fn new(span: Span, _lexer: &dyn NonStreamingLexer<DefaultLexerTypes>) -> Self {
        #[cfg(feature = "code")]
        let val = _lexer.span_str(span).to_string();
        let temp = get_new_temp();
        #[cfg(feature = "code")]
        let code = {
            let inst = IlocInst::LoadImed(InOut::new("loadI".to_string(), val, temp.clone()));
            vec![inst]
        };
        Self {
            span,
            next: Box::new(ASTNode::None),
            #[cfg(feature = "code")]
            code,
            temp,
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }

    #[cfg(feature = "code")]
    pub fn get_temps(&self) -> Vec<String> {
        let mut temps = vec![self.temp.clone()];
        temps.extend(self.next.get_temps());
        temps
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LitFloat {
    pub span: Span,
    pub next: Box<ASTNode>,
    #[cfg(feature = "code")]
    code: Vec<IlocInst>,
}

impl LitFloat {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
            #[cfg(feature = "code")]
            code: vec![],
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LitChar {
    pub span: Span,
    pub next: Box<ASTNode>,
    #[cfg(feature = "code")]
    code: Vec<IlocInst>,
}

impl LitChar {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
            #[cfg(feature = "code")]
            code: vec![],
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LitBool {
    pub span: Span,
    pub next: Box<ASTNode>,
    #[cfg(feature = "code")]
    code: Vec<IlocInst>,
}

impl LitBool {
    pub fn new(span: Span) -> Self {
        Self {
            span,
            next: Box::new(ASTNode::None),
            #[cfg(feature = "code")]
            code: vec![],
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub span: Span,
    pub next: Box<ASTNode>,
    ty: Type,
    #[cfg(feature = "code")]
    code: Vec<IlocInst>,
    #[cfg(feature = "code")]
    temp: String,
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
            #[cfg(feature = "code")]
            code: vec![],
            #[cfg(feature = "code")]
            temp: "".to_string(),
        }
    }

    pub fn add_next(&mut self, next: Box<ASTNode>) {
        self.next = next.clone();
        #[cfg(feature = "code")]
        self.code.extend(next.code());
    }

    pub fn gen_load(
        &mut self,
        _lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<(), ParsingError> {
        #[cfg(feature = "code")]
        {
            let symbol = get_symbol(self.span, _lexer)?;
            let desloc = symbol.desloc().to_string();
            let reg = get_reg(&symbol);
            self.temp = get_new_temp();
            self.code = vec![IlocInst::LoadDesl(FullOp::new(
                "loadAI".to_string(),
                reg,
                desloc,
                self.temp.clone(),
            ))];
        };
        Ok(())
    }

    #[cfg(feature = "code")]
    pub fn get_temps(&self) -> Vec<String> {
        let mut temps = vec![self.temp.clone()];
        temps.extend(self.next.get_temps());
        temps
    }
}

#[cfg(feature = "code")]
fn return_to_caller_insts() -> Vec<IlocInst> {
    let mut code = vec![];
    let restore_rsp = IlocInst::LoadDesl(FullOp::new(
        "loadAI".to_string(),
        "rfp".to_string(),
        RSP_ADDR.to_string(),
        "rsp".to_string(),
    ));
    let restore_rfp = IlocInst::LoadDesl(FullOp::new(
        "loadAI".to_string(),
        "rfp".to_string(),
        RFP_ADDR.to_string(),
        "rfp".to_string(),
    ));

    let ret_addr_temp = get_new_temp();
    let load_ret_addr = IlocInst::LoadDesl(FullOp::new(
        "loadAI".to_string(),
        "rfp".to_string(),
        RET_ADDR.to_string(),
        ret_addr_temp.clone(),
    ));
    let return_to_caller = IlocInst::Jump(Jump::new("jump".to_string(), ret_addr_temp));
    code.push(load_ret_addr);
    code.push(restore_rsp);
    code.push(restore_rfp);
    code.push(return_to_caller);
    code
}
