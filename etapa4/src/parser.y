/* Integrante: Guilherme Dytz dos Santos */

%start program

%%
program -> Result<ASTNode, ParsingError>:
        element_list { $1 } |
        { empty_node!() } ;

element_list -> Result<ASTNode, ParsingError>:
        function element_list {
                match $1? {
                        ASTNode::FnDeclare(mut node) => {
                                let next_fn = Box::new($2?);
                                node.add_next_fn(next_fn);
                                Ok(ASTNode::FnDeclare(node))
                        }
                        _ => unreachable!(),
                }
        } |
        global_declare element_list { $2 } |
        function        { $1 } |
        global_declare  {
                $1?; // propagate parsing errors
                empty_node!()
        } ;


global_declare -> Result<(), ParsingError>:
        type name_list {
                let ty = $1?;
                for var in $2? {
                        let symbol = SymbolEntry::from_untyped_var(var, ty.clone());
                        SCOPE_STACK.with::<_, Result<(), ParsingError>>(|stack| stack.borrow_mut().add_symbol(symbol))?;
                }
                Ok(())
        } ;

name_list -> Result<Vec<UntypedVar>, ParsingError>:
        ident ',' name_list {
                let ident = $1?;
                let var = UntypedVar::UniVar(UniVar::new(ident.span()?, $lexer));
                let mut var_vec = vec![var];
                var_vec.extend($3?);
                Ok(var_vec)
        } |
        ident '[' multidim ']' ',' name_list {
                let ident = $1?;
                let dims = $3?;
                let var_arr = UntypedVar::ArrVar(ArrVar::new(ident.span()?, dims, $lexer));
                let mut var_vec = vec![var_arr];
                var_vec.extend($6?);
                Ok(var_vec)
        } |
        ident '[' multidim ']' ';' {
                let ident = $1?;
                let dims = $3?;
                let var_arr = UntypedVar::ArrVar(ArrVar::new(ident.span()?, dims, $lexer));
                Ok(vec![var_arr])
        } |
        ident ';' {
                let ident = $1?;
                let var = UntypedVar::UniVar(UniVar::new(ident.span()?, $lexer));
                Ok(vec![var])
        } ;

function -> Result<ASTNode, ParsingError>:
        type ident fun_params command_block  {
                let ident = $2?;
                let comm = Box::new($4?);
                let node = FnDeclare::new($span, comm, ident.span()?);
                Ok(ASTNode::FnDeclare(node))
        } ;

fun_params -> Result<ASTNode, ParsingError>:
        '(' param_list ')' { empty_node!() } |
        '(' ')' { empty_node!() } ;

param_list -> Result<ASTNode, ParsingError>:
        type ident ',' param_list { empty_node!() } |
        type ident  { empty_node!() } ;

command_block -> Result<ASTNode, ParsingError>:
        '{' commands '}' { $2 } |
        '{' '}' { empty_node!() } ;

commands -> Result<ASTNode, ParsingError>:
        command commands {
                let comm = $1?;
                match comm {
                        ASTNode::None => $2,
                        _ => {
                                let next = Box::new($2?);
                                let node = comm.add_next(next)?;
                                Ok(node)
                        },
                }
        } |
        command { $1 } ;

command -> Result<ASTNode, ParsingError>:
        command_block ';'       { $1 } |
        var_declare ';'         { $1 } |
        attrib ';'              { $1 } |
        fun_call ';'            { $1 } |
        return ';'              { $1 } |
        flux_ctrl ';'           { $1 } ;

var_declare -> Result<ASTNode, ParsingError>:
        type ident ',' var_list { empty_node!() } |
        type ident "TK_OC_LE" literals ',' var_list {
                let ident = Box::new($2?);
                let lit = Box::new($4?);
                let next = Some(Box::new($6?));
                let node = VarInit::new($span, ident, lit, next);
                Ok(ASTNode::VarInit(node))
        } |
        type ident { empty_node!() } |
        type ident "TK_OC_LE" literals {
                let ident = Box::new($2?);
                let lit = Box::new($4?);
                let node = VarInit::new($span, ident, lit, None);
                Ok(ASTNode::VarInit(node))
        } ;

var_list -> Result<ASTNode, ParsingError>:
        ident "TK_OC_LE" literals ',' var_list {
                let ident = Box::new($1?);
                let lit = Box::new($3?);
                let next = Some(Box::new($5?));
                let node = VarInit::new($span, ident, lit, next);
                Ok(ASTNode::VarInit(node))
        } |
        ident "TK_OC_LE" literals {
                let ident = Box::new($1?);
                let lit = Box::new($3?);
                let node = VarInit::new($span, ident, lit, None);
                Ok(ASTNode::VarInit(node))
        } |
        ident ',' var_list { $3 } |
        ident { empty_node!() } ;

attrib -> Result<ASTNode, ParsingError>:
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

fun_call -> Result<ASTNode, ParsingError>:
        ident '(' arg_list ')' {
                let expr = Box::new($3?);
                let ident = $1?;
                let node = CommFnCall::new($span, expr, ident.span()?);
                Ok(ASTNode::CommFnCall(node))
        } |
        ident '(' ')'  {
                let ident = $1?;
                let node = CommFnCall::new($span, Box::new(ASTNode::None), ident.span()?);
                Ok(ASTNode::CommFnCall(node))
        } ;

arg_list -> Result<ASTNode, ParsingError>:
        expr ',' arg_list {
                let expr = $1?;
                let next = Box::new($3?);
                let node = expr.add_next(next)?;
                Ok(node)
        } |
        expr  { $1 } ;

return -> Result<ASTNode, ParsingError>:
        "TK_PR_RETURN" expr  {
                let expr = Box::new($2?);
                let node = CommReturn::new($span, expr);
                Ok(ASTNode::CommReturn(node))
        } ;

flux_ctrl -> Result<ASTNode, ParsingError>:
        "TK_PR_IF" '(' expr ')' "TK_PR_THEN" command_block  {
                let expr = Box::new($3?);
                let true_fst_comm = Box::new($6?);
                let node = CommIf::new($span, expr, true_fst_comm, Box::new(ASTNode::None));
                Ok(ASTNode::CommIf(node))
        } |
        "TK_PR_IF" '(' expr ')' "TK_PR_THEN" command_block "TK_PR_ELSE" command_block {
                let expr = Box::new($3?);
                let true_fst_comm = Box::new($6?);
                let false_fst_comm = Box::new($8?);
                let node = CommIf::new($span, expr, true_fst_comm, false_fst_comm);
                Ok(ASTNode::CommIf(node))
        } |
        "TK_PR_WHILE" '(' expr ')' command_block  {
                let expr = Box::new($3?);
                let fst_comm = Box::new($5?);
                let node = CommWhile::new($span, expr, fst_comm);
                Ok(ASTNode::CommWhile(node))
        } ;

exp_list -> Result<ASTNode, ParsingError>:
        exp_list '^' expr {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = ExprIdxNode::new($span, child_left, child_right);
                Ok(ASTNode::ExprIdxNode(node))
        } |
        expr { $1 } ;

expr -> Result<ASTNode, ParsingError>:
        or_op { $1 } ;

or_op -> Result<ASTNode, ParsingError>:
        or_op "TK_OC_OR" and_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprOr(node))
        } |
        and_op  { $1 } ;

and_op -> Result<ASTNode, ParsingError>:
        and_op "TK_OC_AND" neq_eq_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right);
                Ok(ASTNode::ExprAnd(node))
        } |
        neq_eq_op  { $1 } ;

neq_eq_op -> Result<ASTNode, ParsingError>:
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

desig_op -> Result<ASTNode, ParsingError>:
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

sum_min_op -> Result<ASTNode, ParsingError>:
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

mul_div_op -> Result<ASTNode, ParsingError>:
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

inv_op -> Result<ASTNode, ParsingError>:
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

exp_end -> Result<ASTNode, ParsingError>:
        '(' expr ')'    { $2 } |
        operand         { $1 } ;

operand -> Result<ASTNode, ParsingError>:
        ident           { $1 } |
        arr_ident       { $1 } |
        literals        { $1 } |
        fun_call        { $1 } ;

arr_ident -> Result<ASTNode, ParsingError>:
        ident '[' exp_list ']' {
                let ident = Box::new($1?);
                let expr_tree = Box::new($3?);
                let node = ArrIdx::new($span, ident, expr_tree);
                Ok(ASTNode::ArrIdx(node))
        } ;

ident -> Result<ASTNode, ParsingError>:
        "TK_IDENTIFICADOR" { Ok(ASTNode::Identifier(Identifier::new($span)))} ;

literals -> Result<ASTNode, ParsingError>:
        "TK_LIT_INT"      { Ok(ASTNode::LitInt(LitInt::new($span))) } |
        "TK_LIT_FLOAT"    { Ok(ASTNode::LitFloat(LitFloat::new($span))) } |
        "TK_LIT_CHAR"     { Ok(ASTNode::LitChar(LitChar::new($span))) } |
        "TK_LIT_TRUE"     { Ok(ASTNode::LitBool(LitBool::new($span))) } |
        "TK_LIT_FALSE"    { Ok(ASTNode::LitBool(LitBool::new($span))) } ;

type -> Result<SymbolType, ParsingError>:
        "TK_PR_INT" { Ok(SymbolType::INT) } |
        "TK_PR_FLOAT" { Ok(SymbolType::FLOAT) } |
        "TK_PR_BOOL" { Ok(SymbolType::BOOL) } |
        "TK_PR_CHAR"  { Ok(SymbolType::CHAR) } ;

multidim -> Result<Vec<usize>, ParsingError>:
        lit_int_val '^' multidim {
                let mut dim = vec![$1?];
                dim.extend($3?);
                Ok(dim)
        } |
        lit_int_val { Ok(vec![$1?]) } ;

lit_int_val -> Result<usize, ParsingError>:
        "TK_LIT_INT" { int_from_span($span, $lexer) } ;

%%

use etapa4::{ast::{
        ASTNode,
        FnDeclare,
        VarInit,
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
        Identifier},
        errors::ParsingError,
        SCOPE_STACK,
        semantic_aux::{int_from_span,
                       UniVar,
                       ArrVar,
                       UntypedVar,
                       SymbolType,
                       SymbolEntry}};

macro_rules! empty_node {
        () => {
                Ok(ASTNode::None)
        }
}