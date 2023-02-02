/* Integrante: Guilherme Dytz dos Santos */

%start program

%%
program -> (): element_list {} | {} ;

element_list -> (): element_list function {} |
              element_list global_declare {} |
              function {} |
              global_declare  {} ;


global_declare -> (): type name_list  {} ;

name_list -> (): "TK_IDENTIFICADOR" ',' name_list {} |
           "TK_IDENTIFICADOR" '[' multidim ']' ',' name_list {} |
           "TK_IDENTIFICADOR" '[' multidim ']' ';' {} |
           "TK_IDENTIFICADOR" ';'  {} ;

function -> (): type_id fun_params command_block  {} ;

fun_params -> (): '(' param_list ')' {} |
            '(' ')' {} ;

param_list -> (): type_id ',' param_list {} |
            type_id  {} ;

command_block -> (): '{' commands '}' {} |
               '{' '}' {} ;

commands -> (): commands command {} |
          command  {} ;

command -> (): command_block ';' {} |
         var_declare ';' {} |
         attrib ';' {} |
         fun_call ';' {} |
         return ';' {} |
         flux_ctrl ';'  {} ;

var_declare -> (): type_id ',' var_list {} |
             type_id "TK_OC_LE" literals ',' var_list {} |
             type_id {} |
             type_id "TK_OC_LE" literals  {} ;

var_list -> (): "TK_IDENTIFICADOR" ',' var_list {} |
          "TK_IDENTIFICADOR" "TK_OC_LE" literals ',' var_list {} |
          "TK_IDENTIFICADOR" "TK_OC_LE" literals {} |
          "TK_IDENTIFICADOR"  {} ;

attrib -> (): "TK_IDENTIFICADOR" '=' expr {} |
        "TK_IDENTIFICADOR" '[' exp_list ']' '=' expr  {} ;

fun_call -> (): "TK_IDENTIFICADOR" '(' arg_list ')' {} |
          "TK_IDENTIFICADOR" '(' ')'  {} ;

arg_list -> (): expr ',' arg_list {} |
          expr  {} ;

return -> (): "TK_PR_RETURN" expr  {} ;

flux_ctrl -> (): if {} |
           if else {} |
           while  {} ;

if -> (): "TK_PR_IF" '(' expr ')' "TK_PR_THEN" command_block  {} ;
else -> (): "TK_PR_ELSE" command_block  {} ;
while -> (): "TK_PR_WHILE" '(' expr ')' command_block  {} ;

exp_list -> (): exp_list '^' expr {} |
          expr  {} ;

expr -> (): or_op  {} ;

or_op -> (): or_op "TK_OC_OR" and_op {} |
       and_op  {} ;

and_op -> (): and_op "TK_OC_AND" neq_eq_op {} |
        neq_eq_op  {} ;

neq_eq_op -> (): neq_eq_op "TK_OC_EQ" desig_op {} |
           neq_eq_op "TK_OC_NE" desig_op {} |
           desig_op  {} ;

desig_op -> (): desig_op '<' sum_min_op {} |
          desig_op '>' sum_min_op {} |
          desig_op "TK_OC_LE" sum_min_op {} |
          desig_op "TK_OC_GE" sum_min_op {} |
          sum_min_op  {} ;

sum_min_op -> (): sum_min_op '+' mul_div_op {} |
            sum_min_op '-' mul_div_op {} |
            mul_div_op  {} ;

mul_div_op -> (): mul_div_op '*' inv_op {} |
            mul_div_op '/' inv_op {} |
            mul_div_op '%' inv_op {} |
            inv_op  {} ;

inv_op -> (): '-' exp_end {} |
        '!' exp_end {} |
        exp_end  {} ;

exp_end -> (): '(' expr ')' {} |
         operand  {} ;

operand -> (): "TK_IDENTIFICADOR" {} |
         "TK_IDENTIFICADOR" '[' exp_list ']' {} |
         literals {} |
         fun_call  {} ;

literals -> (): "TK_LIT_INT" {} |
          "TK_LIT_FLOAT" {} |
          "TK_LIT_CHAR" {} |
          "TK_LIT_TRUE" {} |
          "TK_LIT_FALSE"  {} ;

type_id -> (): type "TK_IDENTIFICADOR"  {} ;

type -> (): "TK_PR_INT" {} |
      "TK_PR_FLOAT" {} |
      "TK_PR_BOOL" {} |
      "TK_PR_CHAR"  {} ;

multidim -> (): "TK_LIT_INT" '^' multidim {} |
          "TK_LIT_INT" {} ;

%%