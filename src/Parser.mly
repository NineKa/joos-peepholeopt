%token <(Lexing.position * Lexing.position)> EOL EOF

(* instructions *)
%token <(Lexing.position * Lexing.position)>
INST_NOP INST_I2C INST_NEW INST_INSTANCEOF INST_CHECKCAST INST_IMUL INST_INEG
INST_IREM INST_ISUB INST_IDIV INST_IADD INST_IINC INST_GOTO INST_IFEQ INST_IFNE
INST_IF_ACMPEQ INST_IF_ACMPNE INST_IFNULL INST_IFNONNULL INST_IF_ICMPEQ
INST_IF_ICMPGT INST_IF_ICMPLT INST_IF_ICMPLE INST_IF_ICMPGE INST_IF_ICMPNE
INST_IRETURN INST_ARETURN INST_RETURN INST_ALOAD INST_ASTORE INST_ILOAD
INST_ISTORE INST_DUP INST_POP INST_SWAP INST_LDC INST_ACONST_NULL INST_GETFIELD
INST_PUTFIELD INST_INVOKEVIRTUAL INST_INVOKENONVIRTUAL

(* keywords *)
%token <(Lexing.position * Lexing.position)>
KEYWORD_PATTERN KEYWORD_PRECONDITION KEYWORD_DEFINITION KEYWORD_END
KEYWORD_ARROW KEYWORD_SCHEMA KEYWORD_EXTERN

(* literals *)
%token <(Lexing.position * Lexing.position) * string> STRING
%token <(Lexing.position * Lexing.position) * string> METHOD_SPEC
%token <(Lexing.position * Lexing.position) * string> NAME
%token <(Lexing.position * Lexing.position) * int>    NUMERIC

(* operators *)
%token <(Lexing.position * Lexing.position)>
(* ( *) OP_PARAM_LEFT   (* ) *) OP_PARAM_RIGHT
(* [ *) OP_SQUARE_LEFT  (* ] *) OP_SQUARE_RIGHT
(* { *) OP_CURLY_LEFT   (* } *) OP_CURLY_RIGHT
(* < *) OP_ANGULAR_LEFT (* > *) OP_ANGULAR_RIGHT
(* = *) OP_ASSIGN
(* + *) OP_PLUS (* - *) OP_DASH (* * *) OP_STAR (* / *) OP_SLASH (* % *) OP_REM
(* || *) OP_OR  (* && *) OP_AND
(* == *) OP_EQ  (* != *) OP_NE  (* >  *) OP_GT  (* <  *) OP_LT
(* >= *) OP_GE  (* <= *) OP_LE
(* , *) OP_COMMA
(* : *) OP_COLON

%type <AST.compilation_unit> compilation_unit
                 
%start compilation_unit

%left OP_OR
%left OP_AND
%left OP_EQ OP_NE
%left OP_GT OP_LT OP_GE OP_LE
%left OP_PLUS OP_DASH
%left OP_STAR OP_SLASH OP_REM
%nonassoc EXPR_UPGRADE

%%

compilation_unit: eol compilation_unit_core  { $2 }
  | compilation_unit_core                    { $1 }
;

compilation_unit_core: EOF
    { AST.make_compilation_unit_new () }
  | pattern compilation_unit_core
    { AST.make_compilation_unit_add_pattern $2 $1 }
  | schema compilation_unit_core
    { AST.make_compilation_unit_add_schema  $2 $1 }
;

(* schema start here *)

schema: KEYWORD_SCHEMA eol schema_core KEYWORD_END eol
    {
      let (start_pos, _) = $1 in
      let (_, end_pos) = $4 in
      AST.make_schema (start_pos, end_pos) $3
    };

schema_core: schema_content    { [ $1 ]      }
  | schema_core schema_content { $1 @ [ $2 ] }
;

schema_content: NAME eol
    {
      let ((start_pos, end_pos), name) = $1 in
      AST.make_schema_element_internal (start_pos, end_pos) name
    }
  | KEYWORD_EXTERN NAME eol
    {
      let (start_pos, _) = $1 in
      let ((_, end_pos), name) = $2 in
      AST.make_schema_element_external (start_pos, end_pos) name
    } 
;

(* patterns start here *)

pattern_header: KEYWORD_PATTERN NAME eol
    {
      let (start_pos, _) = $1 in
      let ((_, end_pos), name) = $2 in
      AST.make_pattern_new (start_pos, end_pos) name
    } ;

pattern_prelude: pattern_header
    { $1 }
  | pattern_prelude definition eol
    { AST.make_pattern_add_definition $1 $2   }
  | pattern_prelude precondition eol
    { AST.make_pattern_add_precondition $1 $2 }
;

pattern_capture: pattern_prelude body eol
    { AST.make_pattern_add_capture_pattern $1 $2 }
  | pattern_capture body eol
    { AST.make_pattern_add_capture_pattern $1 $2 }
;

pattern_rewrite: pattern_capture KEYWORD_ARROW eol
    { $1 }
  | pattern_rewrite body eol
    { AST.make_pattern_add_rewrite_pattern $1 $2 }
;

pattern: pattern_rewrite KEYWORD_END eol
    {
      let (_, end_pos) = $2 in 
      AST.make_pattern_finalize $1 end_pos
    };

definition: KEYWORD_DEFINITION NAME OP_ASSIGN OP_CURLY_LEFT instructions OP_CURLY_RIGHT
    {
      let (start_pos, _) = $1 in
      let (_, end_pos) = $6 in
      let ((_, _), name) = $2 in
      AST.make_definition (start_pos, end_pos) name $5 
    };

precondition: KEYWORD_PRECONDITION extended_expression { $2 };

body: operand OP_COLON
    {
      let (_, end_pos) = $2 in
      AST.make_statement_label end_pos $1
    }
  | OP_ANGULAR_LEFT OP_SQUARE_LEFT NAME OP_SQUARE_RIGHT OP_ANGULAR_RIGHT
    {
      let (start_pos, _) = $1 in
      let (_, end_pos) = $5 in
      let ((_, _), content) = $3 in
      AST.make_statement_block_one_stmt (start_pos, end_pos) content
    }
  | OP_ANGULAR_LEFT OP_SQUARE_LEFT NAME OP_SQUARE_RIGHT OP_PLUS OP_ANGULAR_RIGHT
    {
      let (start_pos, _) = $1 in
      let (_, end_pos) = $6 in
      let ((_, _), content) = $3 in
      AST.make_statement_block_one_or_more_stmt (start_pos, end_pos) content
    }
  | OP_ANGULAR_LEFT OP_SQUARE_LEFT NAME OP_SQUARE_RIGHT OP_STAR OP_ANGULAR_RIGHT
    {
      let (start_pos, _) = $1 in
      let (_, end_pos) = $6 in
      let ((_, _), content) = $3 in
      AST.make_statement_block_zero_or_more_stmt (start_pos, end_pos) content
    }
  | instruction
    { AST.make_statement_instruction_single $1 }
  | instruction expression
    {
      let instruction_lexical_info = AST.get_instruction_lexical_info $1 in
      let expression_lexical_info = AST.get_expression_lexical_info $2 in
      let start_pos = instruction_lexical_info.start_pos in
      let end_pos = expression_lexical_info.end_pos in
      AST.make_statement_instruction_with_operands (start_pos, end_pos) $1 [$2]
    }
  | instruction OP_CURLY_LEFT extended_expression OP_CURLY_RIGHT
    {
      let instruction_lexical_info = AST.get_instruction_lexical_info $1 in
      let start_pos = instruction_lexical_info.start_pos in
      let (_, end_pos) = $4 in
      AST.make_statement_instruction_with_operands (start_pos, end_pos) $1 [$3]
    }
  | instruction expressions_space
    { AST.make_statement_instruction_with_operands_infer_pos $1 $2 }
  | operand
    { AST.make_statement_instruction_from_operand $1 }
  | expressions_space
    { AST.make_statement_instruction_from_expressions $1 }
;


instruction: INST_NOP     { AST.make_instruction_nop $1              }
  | INST_I2C              { AST.make_instruction_i2c $1              }
  | INST_NEW              { AST.make_instruction_new $1              }
  | INST_INSTANCEOF       { AST.make_instruction_instanceof $1       }
  | INST_CHECKCAST        { AST.make_instruction_checkcast $1        }
  | INST_IMUL             { AST.make_instruction_imul $1             }
  | INST_INEG             { AST.make_instruction_ineg $1             }
  | INST_IREM             { AST.make_instruction_irem $1             }
  | INST_ISUB             { AST.make_instruction_isub $1             }
  | INST_IDIV             { AST.make_instruction_idiv $1             }
  | INST_IADD             { AST.make_instruction_iadd $1             }
  | INST_IINC             { AST.make_instruction_iinc $1             }
  | INST_GOTO             { AST.make_instruction_goto $1             }
  | INST_IFEQ             { AST.make_instruction_ifeq $1             }
  | INST_IFNE             { AST.make_instruction_ifne $1             }
  | INST_IF_ACMPEQ        { AST.make_instruction_if_acmpeq $1        }
  | INST_IF_ACMPNE        { AST.make_instruction_if_acmpne $1        }
  | INST_IFNULL           { AST.make_instruction_ifnull $1           }
  | INST_IFNONNULL        { AST.make_instruction_ifnonnull $1        }
  | INST_IF_ICMPEQ        { AST.make_instruction_if_icmpeq $1        }
  | INST_IF_ICMPGT        { AST.make_instruction_if_icmpgt $1        }
  | INST_IF_ICMPLT        { AST.make_instruction_if_icmplt $1        }
  | INST_IF_ICMPLE        { AST.make_instruction_if_icmple $1        }
  | INST_IF_ICMPGE        { AST.make_instruction_if_icmpge $1        }
  | INST_IF_ICMPNE        { AST.make_instruction_if_icmpne $1        }
  | INST_IRETURN          { AST.make_instruction_ireturn $1          }
  | INST_ARETURN          { AST.make_instruction_areturn $1          }
  | INST_RETURN           { AST.make_instruction_return $1           }
  | INST_ALOAD            { AST.make_instruction_aload $1            }
  | INST_ASTORE           { AST.make_instruction_astore $1           }
  | INST_ILOAD            { AST.make_instruction_iload $1            }
  | INST_ISTORE           { AST.make_instruction_istore $1           }
  | INST_DUP              { AST.make_instruction_dup $1              }
  | INST_POP              { AST.make_instruction_pop $1              }
  | INST_SWAP             { AST.make_instruction_swap $1             }
  | INST_LDC              { AST.make_instruction_ldc $1              }
  | INST_ACONST_NULL      { AST.make_instruction_aconst_null $1      }
  | INST_GETFIELD         { AST.make_instruction_getfield $1         }
  | INST_PUTFIELD         { AST.make_instruction_putfield $1         }
  | INST_INVOKEVIRTUAL    { AST.make_instruction_invokevirtual $1    }
  | INST_INVOKENONVIRTUAL { AST.make_instruction_invokenonvirtual $1 }
;

instructions: instruction                   { [ $1 ]   }
  | instruction OP_COMMA instructions       { $1 :: $3 }
;

expression: operand                         { AST.make_expression_operand $1 }
  | OP_PARAM_LEFT expression OP_PARAM_RIGHT { $2                             }
  | expression OP_PLUS  expression          { AST.make_expression_add $1 $3  }
  | expression OP_DASH  expression          { AST.make_expression_sub $1 $3  }
  | expression OP_STAR  expression          { AST.make_expression_mul $1 $3  }
  | expression OP_SLASH expression          { AST.make_expression_div $1 $3  }
  | expression OP_REM   expression          { AST.make_expression_rem $1 $3  }
  | expression OP_OR    expression          { AST.make_expression_or  $1 $3  }
  | expression OP_AND   expression          { AST.make_expression_and $1 $3  }
  | expression OP_EQ    expression          { AST.make_expression_eq  $1 $3  }
  | expression OP_NE    expression          { AST.make_expression_ne  $1 $3  }
  | expression OP_GT    expression          { AST.make_expression_gt  $1 $3  }
  | expression OP_LT    expression          { AST.make_expression_lt  $1 $3  }
  | expression OP_GE    expression          { AST.make_expression_ge  $1 $3  }
  | expression OP_LE    expression          { AST.make_expression_le  $1 $3  }
;

extended_expression: expression             %prec EXPR_UPGRADE
    { $1 }
  | NAME OP_PARAM_LEFT extended_expressions OP_PARAM_RIGHT
    {
      let ((start_pos, _), target_name) = $1 in
      let operands = $3 in
      let (_, end_pos) = $4 in
      AST.make_expression_invoke (start_pos, end_pos) target_name operands
    }
  | NAME OP_PARAM_LEFT OP_PARAM_RIGHT
    {
      let ((start_pos,_), target_name) = $1 in
      let operands = [] in
      let (_, end_pos) = $3 in
      AST.make_expression_invoke (start_pos, end_pos) target_name operands
    }
  | extended_expression OP_PLUS  extended_expression { AST.make_expression_add $1 $3  }
  | extended_expression OP_DASH  extended_expression { AST.make_expression_sub $1 $3  }
  | extended_expression OP_STAR  extended_expression { AST.make_expression_mul $1 $3  }
  | extended_expression OP_SLASH extended_expression { AST.make_expression_div $1 $3  }
  | extended_expression OP_REM   extended_expression { AST.make_expression_rem $1 $3  }
  | extended_expression OP_OR    extended_expression { AST.make_expression_or  $1 $3  }
  | extended_expression OP_AND   extended_expression { AST.make_expression_and $1 $3  }
  | extended_expression OP_EQ    extended_expression { AST.make_expression_eq  $1 $3  }
  | extended_expression OP_NE    extended_expression { AST.make_expression_ne  $1 $3  }
  | extended_expression OP_GT    extended_expression { AST.make_expression_gt  $1 $3  }
  | extended_expression OP_LT    extended_expression { AST.make_expression_lt  $1 $3  }
  | extended_expression OP_GE    extended_expression { AST.make_expression_ge  $1 $3  }
  | extended_expression OP_LE    extended_expression { AST.make_expression_le  $1 $3  }

;

extended_expressions: extended_expression               { [ $1 ]   }
  | extended_expression OP_COMMA extended_expressions   { $1 :: $3 }
;

expressions_space: expression expression
    { [ $1 ; $2 ] }
  | expression OP_CURLY_LEFT extended_expression OP_CURLY_RIGHT
    { [ $1 ; $3 ] }
  | OP_CURLY_LEFT extended_expression OP_CURLY_RIGHT expression
    { [ $2 ; $4 ] }
  | OP_CURLY_LEFT extended_expression OP_CURLY_RIGHT OP_CURLY_LEFT extended_expression OP_CURLY_RIGHT
    { [ $2 ; $5 ] }
  | expression expressions_space
    { $1 :: $2    }
  | OP_CURLY_LEFT extended_expression OP_CURLY_RIGHT expressions_space
    { $2 :: $4    }
;

operand: OP_SQUARE_LEFT NAME OP_SQUARE_RIGHT
    {
      let (start_pos, _) = $1 in
      let (_, content)   = $2 in
      let (end_pos, _)   = $3 in
      AST.make_capture_name (start_pos, end_pos) content
    }
  | NAME
    {
      let ((start_pos, end_pos), content) = $1 in
      AST.make_name (start_pos, end_pos) content
    }
  | STRING
    {
      let ((start_pos, end_pos), content) = $1 in
      AST.make_string (start_pos, end_pos) content
    }
  | METHOD_SPEC
    {
      let ((start_pos, end_pos), content) = $1 in
      AST.make_method_spec (start_pos, end_pos) content 
    }
  | NUMERIC
    {
      let ((start_pos, end_pos), content) = $1 in
      AST.make_numeric (start_pos, end_pos) content
    }
;

eol: EOL     {()}
  | EOL eol  {()}
;
