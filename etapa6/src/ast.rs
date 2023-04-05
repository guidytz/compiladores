use cfgrammar::Span;
use lrlex::{DefaultLexerTypes, LRNonStreamingLexer};
use lrpar::NonStreamingLexer;

use crate::{
    errors::ParsingError,
    get_symbol,
    semantic_aux::{try_coersion, Type},
};

#[cfg(feature = "code")]
use crate::{get_fn_size, get_new_temp, save_regs};

#[cfg(feature = "code")]
use crate::asm_aux::{
    patch_returns, CmpReg, Directive, Mov, Not, StackInst, RETURN_AND_RBP_OFFSET,
};

#[cfg(feature = "code")]
use crate::{
    asm_aux::{AsmInst, CmpInst, FullOp, In2Out, InOut, Jump},
    get_fn_label, get_new_label, get_val, get_var_deslocs,
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
    ExprInv(InvSigOp),
    LitInt(LitInt),
    LitFloat(LitFloat),
    LitChar(LitChar),
    LitBool(LitBool),
    Identifier(Identifier),
    #[cfg(feature = "code")]
    None(Vec<AsmInst>),
    #[cfg(not(feature = "code"))]
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
        #[cfg(feature = "code")]
        if let ASTNode::None(_) = *self {
            // Empty node has no label as it is not essentially a real node
            return "".to_string();
        }
        #[cfg(not(feature = "code"))]
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
            ASTNode::ExprNeg(node) => {
                str += &node.child.parent_string(&self);
                str += &node.next.parent_string(&self);

                str += &node.child.to_string(lexer);
                str += &node.next.to_string(lexer);
            }
            ASTNode::ExprInv(node) => {
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
            #[cfg(feature = "code")]
            ASTNode::None(_) => { /* NOT A REAL NODE */ }
            #[cfg(not(feature = "code"))]
            ASTNode::None => { /* NOT A REAL NODE */ }
        }
        str
    }

    fn parent_string(&self, parent: &ASTNode) -> String {
        #[cfg(feature = "code")]
        if let ASTNode::None(_) = *self {
            // Empty node has no label as it is not essentially a real node
            return "".to_string();
        }
        #[cfg(not(feature = "code"))]
        if *self == ASTNode::None {
            // Empty node has no label as it is not essentially a real node
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
            #[cfg(feature = "code")]
            ASTNode::None(_) => "".to_owned(),
            #[cfg(not(feature = "code"))]
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
            #[cfg(feature = "code")]
            ASTNode::None(_) => "".to_owned(),
            #[cfg(not(feature = "code"))]
            ASTNode::None => "".to_owned(),
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
            #[cfg(feature = "code")]
            ASTNode::None(_) => Err(ParsingError::SpanError(
                "Empty node does not have a span".to_string(),
            )),
            #[cfg(not(feature = "code"))]
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
            #[cfg(feature = "code")]
            ASTNode::None(_) => Type::UNKNOWN,
            #[cfg(not(feature = "code"))]
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
    pub fn code(&self) -> Vec<AsmInst> {
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
                    AsmInst::Cmp(mut inst) => {
                        inst.name = "je".to_string();
                        AsmInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprNeq(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    AsmInst::Cmp(mut inst) => {
                        inst.name = "jne".to_string();
                        AsmInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprLt(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    AsmInst::Cmp(mut inst) => {
                        inst.name = "jl".to_string();
                        AsmInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprGt(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    AsmInst::Cmp(mut inst) => {
                        inst.name = "jg".to_string();
                        AsmInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprLe(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    AsmInst::Cmp(mut inst) => {
                        inst.name = "jle".to_string();
                        AsmInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprGe(node) => node
                .code
                .clone()
                .into_iter()
                .map(|inst| match inst {
                    AsmInst::Cmp(mut inst) => {
                        inst.name = "jge".to_string();
                        AsmInst::Cmp(inst)
                    }
                    inst => inst,
                })
                .collect(),
            ASTNode::ExprOr(node) => {
                let mut code = node.code.clone();
                let mut inst = code.pop();
                inst = match inst {
                    Some(mut inst) => {
                        inst.add_arithm_inst("orl".to_string());
                        Some(inst)
                    }
                    None => unreachable!("There should be code in code list"),
                };
                code.push(inst.unwrap());
                code
            }
            ASTNode::ExprAnd(node) => {
                let mut code = node.code.clone();
                let mut inst = code.pop();
                inst = match inst {
                    Some(mut inst) => {
                        inst.add_arithm_inst("andl".to_string());
                        Some(inst)
                    }
                    None => unreachable!("There should be code in code list"),
                };
                code.push(inst.unwrap());
                code
            }
            ASTNode::ExprAdd(node) => {
                let mut code = node.code.clone();
                let mut inst = code.pop();
                inst = match inst {
                    Some(mut inst) => {
                        inst.add_arithm_inst("addl".to_string());
                        Some(inst)
                    }
                    None => unreachable!("There should be code in code list"),
                };
                code.push(inst.unwrap());
                code
            }
            ASTNode::ExprSub(node) => {
                let mut code = node.code.clone();
                let mut inst = code.pop();
                inst = match inst {
                    Some(mut inst) => {
                        inst.add_arithm_inst("subl".to_string());
                        Some(inst)
                    }
                    None => unreachable!("There should be code in code list"),
                };
                code.push(inst.unwrap());
                code
            }
            ASTNode::ExprMul(node) => {
                let mut code = node.code.clone();
                let mut inst = code.pop();
                inst = match inst {
                    Some(mut inst) => {
                        inst.add_arithm_inst("imull".to_string());
                        Some(inst)
                    }
                    None => unreachable!("There should be code in code list"),
                };
                code.push(inst.unwrap());
                code
            }
            ASTNode::ExprDiv(node) => {
                let mut code = node.code.clone();
                let mut inst = code.pop();
                inst = match inst {
                    Some(mut inst) => {
                        inst.add_arithm_inst("divl".to_string());
                        Some(inst)
                    }
                    None => unreachable!("There should be code in code list"),
                };
                code.push(inst.unwrap());
                code
            }
            ASTNode::ExprMod(node) => node.code.clone(),
            ASTNode::ExprNeg(node) => node.code.clone(),
            ASTNode::ExprInv(node) => node.code.clone(),
            ASTNode::LitInt(node) => node.code.clone(),
            ASTNode::LitFloat(node) => node.code.clone(),
            ASTNode::LitChar(node) => node.code.clone(),
            ASTNode::LitBool(node) => node.code.clone(),
            ASTNode::Identifier(node) => node.code.clone(),
            ASTNode::None(code) => code.clone(),
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
            ASTNode::ExprNeg(node) => node.child.temp(),
            ASTNode::ExprInv(node) => node.child.temp(),
            ASTNode::FnDeclare(_) => unimplemented!("temp not implemented!"),
            ASTNode::VarInit(_) => unimplemented!("temp not implemented!"),
            ASTNode::CommAttrib(_) => unimplemented!("temp not implemented!"),
            ASTNode::ArrIdx(_) => unimplemented!("temp not implemented!"),
            ASTNode::ExprIdxNode(_) => unimplemented!("temp not implemented!"),
            ASTNode::CommReturn(_) => unimplemented!("temp not implemented!"),
            ASTNode::CommIf(_) => unimplemented!("temp not implemented!"),
            ASTNode::CommWhile(_) => unimplemented!("temp not implemented!"),
            ASTNode::ExprMod(_) => unimplemented!("temp not implemented!"),
            ASTNode::LitFloat(_) => unimplemented!("temp not implemented!"),
            ASTNode::LitChar(_) => unimplemented!("temp not implemented!"),
            ASTNode::LitBool(_) => unimplemented!("temp not implemented!"),
            ASTNode::None(_) => unimplemented!("temp not implemented!"),
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

    #[cfg(feature = "code")]
    pub fn extend_code(self, mut next_code: Vec<AsmInst>) -> Self {
        match self {
            ASTNode::FnDeclare(mut node) => {
                node.extend_code(next_code);
                ASTNode::FnDeclare(node)
            }
            ASTNode::None(code) => {
                next_code.extend(code);
                ASTNode::None(next_code)
            }
            node => node,
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
    pub code: Vec<AsmInst>,
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
            let reserved_size = get_fn_size(label.clone())?;

            let mut code = vec![];

            let text = AsmInst::Directive(Directive::new("text".to_string(), None, None));
            let global_declr = AsmInst::Directive(Directive::new(
                "globl".to_string(),
                Some(label.clone()),
                None,
            ));
            let type_declr = AsmInst::Directive(Directive::new(
                "type".to_string(),
                Some(label.clone()),
                Some("@function".to_string()),
            ));

            let push_rbp =
                AsmInst::StackInst(StackInst::new("pushq".to_string(), "%rbp".to_string()))
                    .add_label(label.clone());

            let update_rfp = AsmInst::MovImed(InOut::new(
                "movq".to_string(),
                "%rsp".to_string(),
                "%rbp".to_string(),
            ));

            let locals_reserve = AsmInst::MovImed(InOut::new(
                "subq".to_string(),
                format!("${reserved_size}"),
                "%rsp".to_string(),
            ));

            let size_declr = AsmInst::Directive(Directive::new(
                "size".to_string(),
                Some(label.clone()),
                Some(format!(".-{}", label)),
            ));

            code.push(text);
            code.push(global_declr);
            code.push(type_declr);
            code.push(push_rbp);
            code.push(update_rfp);
            code.push(locals_reserve);
            code.extend(save_regs());

            code.extend(comm.code());

            code.extend(return_to_caller_insts(label.clone()));
            code.push(size_declr);
            patch_returns(&label, code)
        };

        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;

        Ok(Self {
            span,
            comm,
            next_fn: Box::new(node),
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

    #[cfg(feature = "code")]
    pub fn extend_code(&mut self, mut code: Vec<AsmInst>) {
        code.extend(self.code.clone());
        self.code = code;
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
    pub code: Vec<AsmInst>,
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
                    #[cfg(feature = "code")]
                    ASTNode::None(_) => Some(next.clone()),
                    #[cfg(not(feature = "code"))]
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

            let val = get_val(&symbol);
            let inst = AsmInst::Mov(Mov::new("movl".to_string(), self.lit.temp(), val));
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
    pub code: Vec<AsmInst>,
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
            let val = get_val(&symbol);
            let inst = AsmInst::Mov(Mov::new("movl".to_string(), expr.temp(), val));
            let mut code = vec![];
            code.extend(expr.code());
            code.push(inst);
            code
        };

        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;

        Ok(Self {
            span,
            ident,
            expr,
            next: Box::new(node),
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
    pub code: Vec<AsmInst>,
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
        let temp = get_new_temp()?;

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
                .zip(deslocs)
                .for_each(|(temp, (_key, desloc))| {
                    #[cfg(feature = "debug")]
                    println!("{temp} -> {_key}: {desloc}");

                    let load_temp_to_param = AsmInst::StoreDesl(In2Out::new(
                        "movl".to_string(),
                        temp,
                        "%rsp".to_string(),
                        format!("-{}", desloc + RETURN_AND_RBP_OFFSET),
                    ));
                    code.push(load_temp_to_param);
                });

            let fn_label = get_fn_label(name)?;
            let call_fn = AsmInst::StackInst(StackInst::new("call".to_string(), fn_label));

            code.push(call_fn);

            let retrieve_ret_val = AsmInst::Mov(Mov::new(
                "movl".to_string(),
                "%eax".to_string(),
                temp.clone(),
            ));
            code.push(retrieve_ret_val);

            code
        };

        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;

        Ok(Self {
            span,
            expr,
            name: ident,
            next: Box::new(node),
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
    pub code: Vec<AsmInst>,
}

impl CommReturn {
    pub fn new(span: Span, expr: Box<ASTNode>) -> Self {
        let ty = expr.get_type();

        #[cfg(feature = "code")]
        let code = {
            let mut code = vec![];
            code.extend(expr.code());
            let exp_temp = expr.temp();
            let save_ret_value = AsmInst::Mov(Mov::new(
                "movl".to_string(),
                format!("{exp_temp}"),
                "%eax".to_string(),
            ));
            code.push(save_ret_value);

            let jump_exit = AsmInst::Jump(Jump::new("e_patch".to_string()));
            code.push(jump_exit);

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
    pub code: Vec<AsmInst>,
}

impl CommIf {
    pub fn new(
        span: Span,
        expr: Box<ASTNode>,
        true_fst_comm: Box<ASTNode>,
        false_fst_comm: Box<ASTNode>,
    ) -> Result<Self, ParsingError> {
        let ty = expr.get_type();
        #[cfg(feature = "code")]
        let code = {
            let label_true = get_new_label();
            let label_false = get_new_label();
            let label_later = get_new_label();

            let load_op = AsmInst::Mov(Mov::new(
                "movl".to_string(),
                expr.temp(),
                "%eax".to_string(),
            ));

            let reg = get_new_temp()?;
            let load_reg =
                AsmInst::Mov(Mov::new("movl".to_string(), "$0".to_string(), reg.clone()));
            let cmp_op = AsmInst::CmpReg(CmpReg::new("%eax".to_string(), reg));
            let cmp_ne = AsmInst::Cmp(CmpInst::new("jne".to_string(), label_true.clone()));
            let cbr = AsmInst::Jump(Jump::new(label_false.clone()));
            let true_nop = AsmInst::Nop(Some(label_true));
            let jump_later = AsmInst::Jump(Jump::new(label_later.clone()));
            let false_nop = AsmInst::Nop(Some(label_false));
            let later_nop = AsmInst::Nop(Some(label_later));

            let mut code = vec![];
            code.extend(expr.code());
            code.push(load_op);
            code.push(load_reg);
            code.push(cmp_op);
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

        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;

        Ok(Self {
            span,
            expr,
            true_fst_comm,
            false_fst_comm,
            next: Box::new(node),
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
pub struct CommWhile {
    pub span: Span,
    pub expr: Box<ASTNode>,
    pub fst_comm: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub ty: Type,
    #[cfg(feature = "code")]
    pub code: Vec<AsmInst>,
}

impl CommWhile {
    pub fn new(
        span: Span,
        expr: Box<ASTNode>,
        fst_comm: Box<ASTNode>,
    ) -> Result<Self, ParsingError> {
        let ty = expr.get_type();
        #[cfg(feature = "code")]
        let code = {
            let label_expr = get_new_label();
            let label_true = get_new_label();
            let label_later = get_new_label();

            let nop_expr = AsmInst::Nop(Some(label_expr.clone()));
            let load_op = AsmInst::Mov(Mov::new(
                "movl".to_string(),
                expr.temp(),
                "%eax".to_string(),
            ));

            let reg = get_new_temp()?;
            let load_reg =
                AsmInst::Mov(Mov::new("movl".to_string(), "$0".to_string(), reg.clone()));
            let cmp_op = AsmInst::CmpReg(CmpReg::new("%eax".to_string(), reg));
            let cmp_ne = AsmInst::Cmp(CmpInst::new("bne".to_string(), label_true.clone()));
            let cbr = AsmInst::Jump(Jump::new(label_later.clone()));
            let true_nop = AsmInst::Nop(Some(label_true));
            let jump_back = AsmInst::Jump(Jump::new(label_expr.clone()));
            let later_nop = AsmInst::Nop(Some(label_later));

            let mut code = vec![];
            code.push(nop_expr);
            code.extend(expr.code());
            code.push(load_op);
            code.push(load_reg);
            code.push(cmp_op);
            code.push(cmp_ne);
            code.push(cbr);
            code.push(true_nop);
            code.extend(fst_comm.code());
            code.push(jump_back);
            code.push(later_nop);
            code
        };

        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;

        Ok(Self {
            span,
            expr,
            fst_comm,
            next: Box::new(node),
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
pub struct ArrIdx {
    pub span: Span,
    pub ident: Box<ASTNode>,
    pub expr_tree: Box<ASTNode>,
    pub next: Box<ASTNode>,
    pub ty: Type,
    #[cfg(feature = "code")]
    pub code: Vec<AsmInst>,
}

impl ArrIdx {
    pub fn new(span: Span, ident: Box<ASTNode>, expr_tree: Box<ASTNode>) -> Self {
        let ty = ident.get_type();

        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;

        Self {
            span,
            ident,
            expr_tree,
            next: Box::new(node),
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
    pub code: Vec<AsmInst>,
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
    code: Vec<AsmInst>,
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
        let temp = child_left.temp();
        #[cfg(feature = "code")]
        let code = {
            let inst = AsmInst::Arithm(FullOp::new(
                "".to_string(),
                child_right.temp(),
                child_left.temp(),
            ));
            let mut code = vec![];

            code.extend(child_left.code());
            code.extend(child_right.code());
            code.push(inst);
            code
        };

        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;

        Ok(Self {
            span,
            child_left,
            child_right,
            next: Box::new(node),
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
    code: Vec<AsmInst>,
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
        let temp = get_new_temp()?;

        #[cfg(feature = "code")]
        let code = {
            let label_true = get_new_label();
            let label_false = get_new_label();
            let label_end = get_new_label();

            let load_eax = AsmInst::Mov(Mov::new(
                "movl".to_string(),
                child_left.temp(),
                "%eax".to_string(),
            ));
            let cmp_inst = AsmInst::CmpReg(CmpReg::new("%eax".to_string(), child_right.temp()));
            let jcond = AsmInst::Cmp(CmpInst::new("".to_string(), label_true.clone()));

            let cbr_inst = AsmInst::Jump(Jump::new(label_false.clone()));

            let load_true = AsmInst::MovImed(InOut::new(
                "movl".to_string(),
                "$1".to_string(),
                temp.clone(),
            ))
            .add_label(label_true);

            let jump_later = AsmInst::Jump(Jump::new(label_end.clone()));

            let load_false = AsmInst::MovImed(InOut::new(
                "movl".to_string(),
                "$0".to_string(),
                temp.clone(),
            ))
            .add_label(label_false);

            let nop_inst = AsmInst::Nop(Some(label_end));

            let mut code = vec![];

            code.extend(child_left.code());
            code.extend(child_right.code());
            code.push(load_eax);
            code.push(cmp_inst);
            code.push(jcond);
            code.push(cbr_inst);
            code.push(load_true);
            code.push(jump_later);
            code.push(load_false);
            code.push(nop_inst);
            code
        };

        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;

        Ok(Self {
            span,
            child_left,
            child_right,
            next: Box::new(node),
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
    code: Vec<AsmInst>,
}

impl UnOp {
    pub fn new(span: Span, child: Box<ASTNode>) -> Result<Self, ParsingError> {
        let ty = child.get_type();
        #[cfg(feature = "code")]
        let next = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let next = ASTNode::None;

        #[cfg(feature = "code")]
        let code = {
            let inst = AsmInst::Not(Not::new(child.temp()));
            let mut code = child.code();
            code.push(inst);
            code
        };
        Ok(Self {
            span,
            child,
            next: Box::new(next),
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
pub struct InvSigOp {
    pub span: Span,
    pub child: Box<ASTNode>,
    pub next: Box<ASTNode>,
    ty: Type,
    #[cfg(feature = "code")]
    code: Vec<AsmInst>,
}

impl InvSigOp {
    pub fn new(span: Span, child: Box<ASTNode>) -> Result<Self, ParsingError> {
        let ty = child.get_type();
        #[cfg(feature = "code")]
        let next = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let next = ASTNode::None;

        #[cfg(feature = "code")]
        let code = {
            let not = AsmInst::Not(Not::new(child.temp()));
            let add_one = AsmInst::Arithm(FullOp::new(
                "addl".to_string(),
                "$1".to_string(),
                child.temp(),
            ));
            let mut code = child.code();
            code.push(not);
            code.push(add_one);
            code
        };
        Ok(Self {
            span,
            child,
            next: Box::new(next),
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
pub struct LitInt {
    pub span: Span,
    pub next: Box<ASTNode>,
    #[cfg(feature = "code")]
    code: Vec<AsmInst>,
    #[cfg(feature = "code")]
    temp: String,
}

impl LitInt {
    pub fn new(
        span: Span,
        _lexer: &dyn NonStreamingLexer<DefaultLexerTypes>,
    ) -> Result<Self, ParsingError> {
        #[cfg(feature = "code")]
        let temp = get_new_temp()?;
        #[cfg(feature = "code")]
        let code = {
            let val = format!("${}", _lexer.span_str(span));
            let inst = AsmInst::MovImed(InOut::new("movl".to_string(), val, temp.clone()));
            vec![inst]
        };

        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;

        Ok(Self {
            span,
            next: Box::new(node),
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
    code: Vec<AsmInst>,
}

impl LitFloat {
    pub fn new(span: Span) -> Self {
        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;
        Self {
            span,
            next: Box::new(node),
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
    code: Vec<AsmInst>,
}

impl LitChar {
    pub fn new(span: Span) -> Self {
        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;
        Self {
            span,
            next: Box::new(node),
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
    code: Vec<AsmInst>,
}

impl LitBool {
    pub fn new(span: Span) -> Self {
        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;
        Self {
            span,
            next: Box::new(node),
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
    code: Vec<AsmInst>,
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

        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;

        Self {
            span,
            next: Box::new(node),
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
            let val = get_val(&symbol);
            self.temp = get_new_temp()?;
            self.code = vec![AsmInst::Mov(Mov::new(
                "movl".to_string(),
                val,
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
fn return_to_caller_insts(fn_name: String) -> Vec<AsmInst> {
    use crate::restore_regs;

    let mut code = vec![AsmInst::Nop(Some(format!("e_{fn_name}")))];
    let leave = AsmInst::SingleInst(None, "leave".to_string());
    let ret = AsmInst::SingleInst(None, "ret".to_string());

    code.extend(restore_regs());
    code.push(leave);
    code.push(ret);
    code
}
