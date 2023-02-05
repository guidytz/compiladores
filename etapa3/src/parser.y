/* Integrante: Guilherme Dytz dos Santos */

%start program

%%
program -> Result<ASTNode, anyhow::Error>:
        element_list { $1 } |
        { Ok(ASTNode::None) } ;

element_list -> Result<ASTNode, anyhow::Error>:
        function element_list {
                match $1? {
                        ASTNode::FnDeclare(mut node) => {
                                let next_fn = Box::new($2?);
                                node.add_next_fn(next_fn);
                                Ok(ASTNode::FnDeclare(node))
                        }
                        _ => bail!("First element in production should be a function"),
                }
        } |
        global_declare element_list { $2 } |
        function        { $1 } |
        global_declare  { $1 } ;


global_declare -> Result<ASTNode, anyhow::Error>:
        type name_list { Ok(ASTNode::None) } ;

name_list -> Result<ASTNode, anyhow::Error>:
        ident ',' name_list { Ok(ASTNode::None) } |
        ident '[' multidim ']' ',' name_list { Ok(ASTNode::None) } |
        ident '[' multidim ']' ';' { Ok(ASTNode::None) } |
        ident ';' { Ok(ASTNode::None) } ;

function -> Result<ASTNode, anyhow::Error>:
        type ident fun_params command_block  {
                let ident = $2?;
                let comm = Box::new($4?);
                let node = FnDeclare::new($span, comm, ident.span()?);
                Ok(ASTNode::FnDeclare(node))
        } ;

fun_params -> Result<ASTNode, anyhow::Error>:
        '(' param_list ')' { Ok(ASTNode::None) } |
        '(' ')' { Ok(ASTNode::None) } ;

param_list -> Result<ASTNode, anyhow::Error>:
        type ident ',' param_list { Ok(ASTNode::None) } |
        type ident  { Ok(ASTNode::None) } ;

command_block -> Result<ASTNode, anyhow::Error>:
        '{' commands '}' { $2 } |
        '{' '}' { Ok(ASTNode::None) } ;

commands -> Result<ASTNode, anyhow::Error>:
        command commands {
                let comm = $1?;
                let next = Box::new($2?);
                let node = comm.add_next(next)?;
                Ok(node)
        } |
        command { $1 } ;

command -> Result<ASTNode, anyhow::Error>:
        command_block ';'       { $1 } |
        var_declare ';'         { $1 } |
        attrib ';'              { $1 } |
        fun_call ';'            { $1 } |
        return ';'              { $1 } |
        flux_ctrl ';'           { $1 } ;

var_declare -> Result<ASTNode, anyhow::Error>:
        type ident ',' var_list { Ok(ASTNode::None) } |
        type ident "TK_OC_LE" literals ',' var_list {
                let ident = Box::new($2?);
                let lit = Box::new($4?);
                let mut node = LitInit::new($span, ident, lit);
                match $6? {
                        ASTNode::None => {}
                        ASTNode::LitInit(next) => {
                                let next = Box::new(ASTNode::LitInit(next));
                                node.add_next(next)
                        },
                        _ => bail!("There should be a lit init node here"),
                }
                Ok(ASTNode::LitInit(node))
        } |
        type ident { Ok(ASTNode::None) } |
        type ident "TK_OC_LE" literals {
                let ident = Box::new($2?);
                let lit = Box::new($4?);
                let node = LitInit::new($span, ident, lit);
                Ok(ASTNode::LitInit(node))
        } ;

var_list -> Result<ASTNode, anyhow::Error>:
        ident "TK_OC_LE" literals ',' var_list {
                let ident = Box::new($1?);
                let lit = Box::new($3?);
                let mut node = LitInit::new($span, ident, lit);
                match $5? {
                        ASTNode::None => {}
                        ASTNode::LitInit(next) => {
                                let next = Box::new(ASTNode::LitInit(next));
                                node.add_next(next)
                        },
                        _ => bail!("There should be a lit init node here"),
                }
                Ok(ASTNode::LitInit(node))
        } |
        ident "TK_OC_LE" literals {
                let ident = Box::new($1?);
                let lit = Box::new($3?);
                let node = LitInit::new($span, ident, lit);
                Ok(ASTNode::LitInit(node))
        } |
        ident ',' var_list { $3 } |
        ident { Ok(ASTNode::None) } ;

attrib -> Result<ASTNode, anyhow::Error>:
        ident '=' expr {
                let ident = Box::new($1?);
                let expr = Box::new($3?);
                let node = CommAttrib::new($span, ident, expr);
                Ok(ASTNode::CommAttrib(node))
        } |
        arr_ident '=' expr  {
                let ident = Box::new($1?);
                let expr = Box::new($3?);
                let node = CommAttrib::new($span, ident, expr);
                Ok(ASTNode::CommAttrib(node))
        } ;

fun_call -> Result<ASTNode, anyhow::Error>:
        ident '(' arg_list ')' {
                let expr = Box::new($3?);
                let node = CommFnCall::new($span, Some(expr));
                Ok(ASTNode::CommFnCall(node))
        } |
        ident '(' ')'  {
                let node = CommFnCall::new($span, None);
                Ok(ASTNode::CommFnCall(node))
        } ;

arg_list -> Result<ASTNode, anyhow::Error>:
        expr ',' arg_list {
                let expr = $1?;
                let next = Box::new($3?);
                let node = expr.add_next(next)?;
                Ok(node)
        } |
        expr  { $1 } ;

return -> Result<ASTNode, anyhow::Error>:
        "TK_PR_RETURN" expr  {
                let expr = Box::new($2?);
                let node = CommReturn::new($span, expr);
                Ok(ASTNode::CommReturn(node))
        } ;

flux_ctrl -> Result<ASTNode, anyhow::Error>:
        "TK_PR_IF" '(' expr ')' "TK_PR_THEN" command_block  {
                let expr = Box::new($3?);
                let true_fst_comm = Box::new($6?);
                let node = CommIf::new($span, expr, true_fst_comm, None);
                Ok(ASTNode::CommIf(node))
        } |
        "TK_PR_IF" '(' expr ')' "TK_PR_THEN" command_block "TK_PR_ELSE" command_block {
                let expr = Box::new($3?);
                let true_fst_comm = Box::new($6?);
                let false_fst_comm = Box::new($8?);
                let node = CommIf::new($span, expr, true_fst_comm, Some(false_fst_comm));
                Ok(ASTNode::CommIf(node))
        } |
        "TK_PR_WHILE" '(' expr ')' command_block  {
                let expr = Box::new($3?);
                let fst_comm = Box::new($5?);
                let node = CommWhile::new($span, expr, fst_comm);
                Ok(ASTNode::CommWhile(node))
        } ;

exp_list -> Result<ASTNode, anyhow::Error>:
        exp_list '^' expr {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = ExprIdxNode::new($span, Some(child_left), child_right);
                Ok(ASTNode::ExprIdxNode(node))
        } |
        expr {
                let child_left = None;
                let child_right = Box::new($1?);
                let node = ExprIdxNode::new($span, child_left, child_right);
                Ok(ASTNode::ExprIdxNode(node))
        } ;

expr -> Result<ASTNode, anyhow::Error>:
        or_op { $1 } ;

or_op -> Result<ASTNode, anyhow::Error>:
        or_op "TK_OC_OR" and_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprOr(node))
        } |
        and_op  { $1 } ;

and_op -> Result<ASTNode, anyhow::Error>:
        and_op "TK_OC_AND" neq_eq_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprAnd(node))
        } |
        neq_eq_op  { $1 } ;

neq_eq_op -> Result<ASTNode, anyhow::Error>:
        neq_eq_op "TK_OC_EQ" desig_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprEq(node))
        } |
        neq_eq_op "TK_OC_NE" desig_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprNeq(node))
        } |
        desig_op  { $1 } ;

desig_op -> Result<ASTNode, anyhow::Error>:
        desig_op '<' sum_min_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprLt(node))
         } |
        desig_op '>' sum_min_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprGt(node))
         } |
        desig_op "TK_OC_LE" sum_min_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprLe(node))
         } |
        desig_op "TK_OC_GE" sum_min_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprGe(node))
         } |
        sum_min_op  { $1 } ;

sum_min_op -> Result<ASTNode, anyhow::Error>:
        sum_min_op '+' mul_div_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprAdd(node))
         } |
        sum_min_op '-' mul_div_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprSub(node))
         } |
        mul_div_op  { $1 } ;

mul_div_op -> Result<ASTNode, anyhow::Error>:
        mul_div_op '*' inv_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprMul(node))
        } |
        mul_div_op '/' inv_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprDiv(node))
        } |
        mul_div_op '%' inv_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprMod(node))
        } |
        inv_op  { $1 } ;

inv_op -> Result<ASTNode, anyhow::Error>:
        '-' exp_end {
                let child = Box::new($2?);
                let node = UnOp::new($span, child);
                Ok(ASTNode::ExprInv(node))
        } |
        '!' exp_end {
                let child = Box::new($2?);
                let node = UnOp::new($span, child);
                Ok(ASTNode::ExprNeg(node))
        } |
        exp_end  { $1 } ;

exp_end -> Result<ASTNode, anyhow::Error>:
        '(' expr ')'    { $2 } |
        operand         { $1 } ;

operand -> Result<ASTNode, anyhow::Error>:
        ident           { $1 } |
        arr_ident       { $1 } |
        literals        { $1 } |
        fun_call        { $1 } ;

arr_ident -> Result<ASTNode, anyhow::Error>:
        ident '[' exp_list ']' {
                let ident = Box::new($1?);
                let expr_tree = Box::new($3?);
                let node = ArrIdx::new($span, ident, expr_tree);
                Ok(ASTNode::ArrIdx(node))
        } ;

ident -> Result<ASTNode, anyhow::Error>:
        "TK_IDENTIFICADOR" { Ok(ASTNode::Identifier(Identifier::new($span)))} ;

literals -> Result<ASTNode, anyhow::Error>:
        "TK_LIT_INT"      { Ok(ASTNode::LitInt(LitInt::new($span))) } |
        "TK_LIT_FLOAT"    { Ok(ASTNode::LitFloat(LitFloat::new($span))) } |
        "TK_LIT_CHAR"     { Ok(ASTNode::LitChar(LitChar::new($span))) } |
        "TK_LIT_TRUE"     { Ok(ASTNode::LitBool(LitBool::new($span))) } |
        "TK_LIT_FALSE"    { Ok(ASTNode::LitBool(LitBool::new($span))) } ;

type -> Result<ASTNode, anyhow::Error>:
        "TK_PR_INT" { Ok(ASTNode::None) } |
        "TK_PR_FLOAT" { Ok(ASTNode::None) } |
        "TK_PR_BOOL" { Ok(ASTNode::None) } |
        "TK_PR_CHAR"  { Ok(ASTNode::None) } ;

multidim -> Result<ASTNode, anyhow::Error>:
        multidim '^' "TK_LIT_INT" { Ok(ASTNode::None) } |
        "TK_LIT_INT" { Ok(ASTNode::None) } ;

%%

use etapa3::ast::{
        ASTNode,
        FnDeclare,
        LitInit,
        CommAttrib,
        CommFnCall,
        CommReturn,
        CommIf,
        CommWhile,
        ArrIdx,
        ExprIdxNode,
        BinOp,
        UnOp,
        LitInt,
        LitFloat,
        LitChar,
        LitBool,
        Identifier};
use anyhow::bail;