/* Integrante: Guilherme Dytz dos Santos */

%start program

%%
program -> Result<ASTNode, ParsingError>:
        element_list {
                Ok($1?)
        } |
        %empty { empty_node() } ;

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
        global_declare element_list {
                let _declr_code = $1?;
                #[cfg(not(feature = "code"))]
                let node = $2?;
                #[cfg(feature = "code")]
                let node = $2?.extend_code(_declr_code);
                Ok(node)
        } |
        function        { $1 } |
        global_declare  {
                let _declr_code = $1?;
                #[cfg(feature = "code")]
                let node = ASTNode::None(_declr_code);
                #[cfg(not(feature = "code"))]
                let node = ASTNode::None;

                Ok(node)
        } ;


global_declare -> Result<Vec<AsmInst>, ParsingError>:
        type name_list {
                let ty = $1?;
                let mut global_declr_code = vec![];
                for var in $2? {
                        let symbol = SymbolEntry::from_untyped_global_declr(var, ty.clone());
                        global_declr_code.extend(gen_global_declr_code(&symbol));
                        add_symbol_to_curr_st(symbol)?;
                }
                Ok(global_declr_code)
        } ;

name_list -> Result<Vec<UntypedGlobalDeclr>, ParsingError>:
        ident ',' name_list {
                let ident = $1?;
                let var = UntypedGlobalDeclr::Var(UntypedVar::new(ident.span()?, $lexer));
                let mut var_vec = vec![var];
                var_vec.extend($3?);
                Ok(var_vec)
        } |
        ident '[' multidim ']' ',' name_list {
                let ident = $1?;
                let dims = $3?;
                let var_arr = UntypedGlobalDeclr::Arr(UntypedArr::new(ident.span()?, dims, $lexer));
                let mut var_vec = vec![var_arr];
                var_vec.extend($6?);
                Ok(var_vec)
        } |
        ident '[' multidim ']' ';' {
                let ident = $1?;
                let dims = $3?;
                let var_arr = UntypedGlobalDeclr::Arr(UntypedArr::new(ident.span()?, dims, $lexer));
                Ok(vec![var_arr])
        } |
        ident ';' {
                let ident = $1?;
                let var = UntypedGlobalDeclr::Var(UntypedVar::new(ident.span()?, $lexer));
                Ok(vec![var])
        } ;

function -> Result<ASTNode, ParsingError>:
        type ident fun_params fn_command_block {
                let ident = $2?;

                let ty = $1?;
                let name = $lexer.span_str(ident.span()?).to_string();
                add_name_to_last_child_table(name.clone());
                let args = $3?;
                let entry = SymbolEntry::Fn(SymbolFn::new(name.clone(), ty, $span, $lexer, args, name));
                add_symbol_to_curr_st(entry)?;
                let comm = Box::new($4?);
                let node = FnDeclare::new($span, comm, ident.span()?, $lexer)?;
                Ok(ASTNode::FnDeclare(node))
        } ;

fun_params -> Result<Option<Vec<SymbolEntry>>, ParsingError>:
        '(' begin_fn_scope param_list ')' {
                let symbols = $3?;
                // let args_size = symbols.iter().map(|symbol| symbol.size()).reduce(|acc, size| acc + size).unwrap_or(0);
                change_base_function_desloc(4);
                Ok(Some(symbols))
        } |
        '(' begin_fn_scope ')' {
                change_base_function_desloc(4);
                Ok(None)
         } ;

param_list -> Result<Vec<SymbolEntry>, ParsingError>:
        type ident ',' param_list {
                let name = $lexer.span_str($2?.span()?).to_string();
                let var = SymbolEntry::Var(CommonAttrs::new(name, $1?, $span, $lexer));
                add_symbol_to_curr_st(var.clone())?;

                let mut list = vec![var];
                list.extend($4?);
                Ok(list)
        } |
        type ident  {
                let name = $lexer.span_str($2?.span()?).to_string();
                let var = SymbolEntry::Var(CommonAttrs::new(name, $1?, $span, $lexer));
                add_symbol_to_curr_st(var.clone())?;

                Ok(vec![var])
        } ;

fn_command_block -> Result<ASTNode, ParsingError>:
        '{' commands end_scope '}' { $2 } |
        '{' end_scope '}' { empty_node() } ;

command_block -> Result<ASTNode, ParsingError>:
        '{' begin_inner_scope commands end_scope '}' { $3 } |
        '{' '}' { empty_node() } ;

commands -> Result<ASTNode, ParsingError>:
        command commands {
                let comm = $1?;
                match comm {
                        #[cfg(feature = "code")]
                        ASTNode::None(_) => $2,
                        #[cfg(not(feature = "code"))]
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
        type var_list {
                let aux = $2?;
                let ty = $1?;
                for var in aux.vars.into_iter().rev() {
                        let symbol = SymbolEntry::from_untyped_var(var, ty.clone());
                        add_symbol_to_curr_st(symbol)?;
                }
                let node = aux.node.update_type(ty, $lexer)?.gen_self_code($lexer)?;
                Ok(node)
        } ;

var_list -> Result<LocalDeclrAux, ParsingError>:
        ident "TK_OC_LE" literals ',' var_list {
                let ident = $1?;
                let mut aux = $5?;

                let var = UntypedVar::new(ident.span()?, $lexer);
                let lit = Box::new($3?);

                aux.vars.push(var);
                let node = VarInit::new($span, Box::new(ident), lit, Some(Box::new(aux.node)));
                aux.node = ASTNode::VarInit(node);
                Ok(aux)
        } |
        ident "TK_OC_LE" literals {
                let ident = $1?;
                let lit = Box::new($3?);
                let var = UntypedVar::new(ident.span()?, $lexer);
                let node = ASTNode::VarInit(VarInit::new($span, Box::new(ident), lit, None));
                Ok(LocalDeclrAux::new(vec![var], node))
        } |
        ident ',' var_list {
                let ident = $1?;
                let mut aux = $3?;
                let var = UntypedVar::new(ident.span()?, $lexer);
                aux.vars.push(var);
                Ok(aux)
        } |
        ident {
                let ident = $1?;
                let vars = vec![UntypedVar::new(ident.span()?, $lexer)];
                Ok(LocalDeclrAux::with_vars(vars))
        } ;

attrib -> Result<ASTNode, ParsingError>:
        checked_ident '=' expr {
                let ident = Box::new($1?);
                let expr = Box::new($3?);
                let node = CommAttrib::new($span, ident, expr, $lexer)?;
                Ok(ASTNode::CommAttrib(node))
        } |
        arr_ident '=' expr  {
                let ident = Box::new($1?);
                let expr = Box::new($3?);
                let node = CommAttrib::new($span, ident, expr, $lexer)?;
                Ok(ASTNode::CommAttrib(node))
        } ;

fun_call -> Result<ASTNode, ParsingError>:
        ident '(' arg_list ')' {
                let expr = Box::new($3?);
                let ident = $1?;
                check_declaration(&ident, $lexer, UsageType::FnCall)?;
                let node = CommFnCall::new($span, expr, Box::new(ident), $lexer)?;
                Ok(ASTNode::CommFnCall(node))
        } |
        ident '(' ')'  {
                let ident = $1?;
                check_declaration(&ident, $lexer, UsageType::FnCall)?;
                #[cfg(feature = "code")]
                let none = ASTNode::None(vec![]);
                #[cfg(not(feature = "code"))]
                let none = ASTNode::None;
                let node = CommFnCall::new($span, Box::new(none), Box::new(ident), $lexer)?;
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
                #[cfg(feature = "code")]
                let none = ASTNode::None(vec![]);
                #[cfg(not(feature = "code"))]
                let none = ASTNode::None;
                let node = CommIf::new($span, expr, true_fst_comm, Box::new(none))?;
                Ok(ASTNode::CommIf(node))
        } |
        "TK_PR_IF" '(' expr ')' "TK_PR_THEN" command_block "TK_PR_ELSE" command_block {
                let expr = Box::new($3?);
                let true_fst_comm = Box::new($6?);
                let false_fst_comm = Box::new($8?);
                let node = CommIf::new($span, expr, true_fst_comm, false_fst_comm)?;
                Ok(ASTNode::CommIf(node))
        } |
        "TK_PR_WHILE" '(' expr ')' command_block  {
                let expr = Box::new($3?);
                let fst_comm = Box::new($5?);
                let node = CommWhile::new($span, expr, fst_comm)?;
                Ok(ASTNode::CommWhile(node))
        } ;

exp_list -> Result<ASTNode, ParsingError>:
        exp_list '^' expr {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = ExprIdxNode::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprIdxNode(node))
        } |
        expr { $1 } ;

expr -> Result<ASTNode, ParsingError>:
        or_op { $1 } ;

or_op -> Result<ASTNode, ParsingError>:
        or_op "TK_OC_OR" and_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprOr(node))
        } |
        and_op  { $1 } ;

and_op -> Result<ASTNode, ParsingError>:
        and_op "TK_OC_AND" neq_eq_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprAnd(node))
        } |
        neq_eq_op  { $1 } ;

neq_eq_op -> Result<ASTNode, ParsingError>:
        neq_eq_op "TK_OC_EQ" desig_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = CmpOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprEq(node))
        } |
        neq_eq_op "TK_OC_NE" desig_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = CmpOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprNeq(node))
        } |
        desig_op  { $1 } ;

desig_op -> Result<ASTNode, ParsingError>:
        desig_op '<' sum_min_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = CmpOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprLt(node))
         } |
        desig_op '>' sum_min_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = CmpOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprGt(node))
         } |
        desig_op "TK_OC_LE" sum_min_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = CmpOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprLe(node))
         } |
        desig_op "TK_OC_GE" sum_min_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = CmpOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprGe(node))
         } |
        sum_min_op  { $1 } ;

sum_min_op -> Result<ASTNode, ParsingError>:
        sum_min_op '+' mul_div_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprAdd(node))
         } |
        sum_min_op '-' mul_div_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprSub(node))
         } |
        mul_div_op  { $1 } ;

mul_div_op -> Result<ASTNode, ParsingError>:
        mul_div_op '*' inv_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprMul(node))
        } |
        mul_div_op '/' inv_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprDiv(node))
        } |
        mul_div_op '%' inv_op {
                let child_left = Box::new($1?);
                let child_right = Box::new($3?);
                let node = BinOp::new($span, child_left, child_right, $lexer)?;
                Ok(ASTNode::ExprMod(node))
        } |
        inv_op  { $1 } ;

inv_op -> Result<ASTNode, ParsingError>:
        '-' exp_end {
                let child = Box::new($2?);
                let node = InvSigOp::new($span, child)?;
                Ok(ASTNode::ExprInv(node))
        } |
        '!' exp_end {
                let child = Box::new($2?);
                let node = UnOp::new($span, child)?;
                Ok(ASTNode::ExprNeg(node))
        } |
        exp_end  { $1 } ;

exp_end -> Result<ASTNode, ParsingError>:
        '(' expr ')'    { $2 } |
        operand         { $1 } ;

operand -> Result<ASTNode, ParsingError>:
        checked_ident   {
                let mut ident = $1?;
                ident.gen_load($lexer)?;
                Ok(ident)
        } |
        arr_ident       { $1 } |
        literals        { $1 } |
        fun_call        { $1 } ;

checked_ident -> Result<ASTNode, ParsingError>:
        ident {
                let ident = $1?;
                check_declaration(&ident, $lexer, UsageType::Var)?;
                Ok(ident)
        } ;

arr_ident -> Result<ASTNode, ParsingError>:
        ident '[' exp_list ']' {
                let ident = Box::new($1?);
                check_declaration(&ident, $lexer, UsageType::Arr)?;
                let expr_tree = Box::new($3?);
                let node = ArrIdx::new($span, ident, expr_tree);
                Ok(ASTNode::ArrIdx(node))
        } ;

ident -> Result<ASTNode, ParsingError>:
        "TK_IDENTIFICADOR" { Ok(ASTNode::Identifier(Identifier::new($span, $lexer)))} ;

literals -> Result<ASTNode, ParsingError>:
        "TK_LIT_INT" {
                let lit_entry = SymbolEntry::from_lit_span($span, $lexer);
                add_symbol_to_curr_st(lit_entry)?;
                Ok(ASTNode::LitInt(LitInt::new($span, $lexer)))
        } |
        "TK_LIT_FLOAT" {
                let lit_entry = SymbolEntry::from_lit_span($span, $lexer);
                add_symbol_to_curr_st(lit_entry)?;
                Ok(ASTNode::LitFloat(LitFloat::new($span)))
        } |
        "TK_LIT_CHAR" {
                let lit_entry = SymbolEntry::from_lit_span($span, $lexer);
                add_symbol_to_curr_st(lit_entry)?;
                Ok(ASTNode::LitChar(LitChar::new($span)))
        } |
        "TK_LIT_TRUE" {
                let lit_entry = SymbolEntry::from_lit_span($span, $lexer);
                add_symbol_to_curr_st(lit_entry)?;
                Ok(ASTNode::LitBool(LitBool::new($span)))
        } |
        "TK_LIT_FALSE" {
                let lit_entry = SymbolEntry::from_lit_span($span, $lexer);
                add_symbol_to_curr_st(lit_entry)?;
                Ok(ASTNode::LitBool(LitBool::new($span)))
        } ;

type -> Result<Type, ParsingError>:
        "TK_PR_INT" { Ok(Type::INT) } |
        "TK_PR_FLOAT" { Ok(Type::FLOAT) } |
        "TK_PR_BOOL" { Ok(Type::BOOL) } |
        "TK_PR_CHAR"  { Ok(Type::CHAR) } ;

multidim -> Result<Vec<u32>, ParsingError>:
        lit_int_val '^' multidim {
                let mut dim = vec![$1?];
                dim.extend($3?);
                Ok(dim)
        } |
        lit_int_val { Ok(vec![$1?]) } ;

lit_int_val -> Result<u32, ParsingError>:
        "TK_LIT_INT" {
                let lit_entry = SymbolEntry::from_lit_span($span, $lexer);
                add_symbol_to_curr_st(lit_entry)?;
                int_from_span($span, $lexer)
        } ;

begin_inner_scope -> Result<(), ParsingError>:
        %empty {
                new_scope(ScopeType::Inner);
                Ok(())
        };

begin_fn_scope -> Result<(), ParsingError>:
        %empty {
                new_scope(ScopeType::Fn);
                Ok(())
        };

end_scope -> Result<(), ParsingError>:
        %empty {
                end_scope();
                Ok(())
        };

%%

use etapa6::{ast::{
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
        CmpOp,
        BinOp,
        UnOp,
        InvSigOp,
        LitInt,
        LitFloat,
        LitChar,
        LitBool,
        Identifier},
        errors::ParsingError,
        add_symbol_to_curr_st,
        add_name_to_last_child_table,
        change_base_function_desloc,
        new_scope,
        end_scope,
        asm_aux::{AsmInst, gen_global_declr_code},
        semantic_aux::{int_from_span,
                       UntypedVar,
                       UntypedArr,
                       UntypedGlobalDeclr,
                       Type,
                       SymbolEntry,
                       SymbolFn,
                       CommonAttrs,
                       LocalDeclrAux,
                       UsageType,
                       ScopeType,
                       check_declaration}};

fn empty_node() -> Result<ASTNode, ParsingError> {
        #[cfg(feature = "code")]
        let node = ASTNode::None(vec![]);
        #[cfg(not(feature = "code"))]
        let node = ASTNode::None;
        Ok(node)
}