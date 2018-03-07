let prettyprint_token_info = fun (token_info :Lexing.position * Lexing.position) ->
  let start_pos, end_pos = token_info in
  assert (start_pos.Lexing.pos_fname == end_pos.Lexing.pos_fname) ;
  let filename = if (String.length start_pos.Lexing.pos_fname) == 0 then
                   "*unknown*"
                 else
                   start_pos.Lexing.pos_fname
  in
  Printf.sprintf "(%d:%d - %d:%d @ %s)"
    start_pos.Lexing.pos_lnum (start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol)
    end_pos.Lexing.pos_lnum (end_pos.Lexing.pos_cnum - end_pos.Lexing.pos_bol)
    filename

let prettyprint_token = fun (token :Parser.token) ->
  match token with
  (* instructions *)
  | Parser.INST_NOP(pos)              -> Printf.sprintf "INST_NOP %s"              (prettyprint_token_info pos)
  | Parser.INST_I2C(pos)              -> Printf.sprintf "INST_I2C %s"              (prettyprint_token_info pos)
  | Parser.INST_NEW(pos)              -> Printf.sprintf "INST_NEW %s"              (prettyprint_token_info pos)
  | Parser.INST_INSTANCEOF(pos)       -> Printf.sprintf "INST_INSTANCEOF %s"       (prettyprint_token_info pos)
  | Parser.INST_CHECKCAST(pos)        -> Printf.sprintf "INST_CHECKCAST %s"        (prettyprint_token_info pos)
  | Parser.INST_IMUL(pos)             -> Printf.sprintf "INST_IMUL %s"             (prettyprint_token_info pos)
  | Parser.INST_INEG(pos)             -> Printf.sprintf "INST_INEG %s"             (prettyprint_token_info pos)
  | Parser.INST_IREM(pos)             -> Printf.sprintf "INST_IREM %s"             (prettyprint_token_info pos)
  | Parser.INST_ISUB(pos)             -> Printf.sprintf "INST_ISUB %s"             (prettyprint_token_info pos)
  | Parser.INST_IDIV(pos)             -> Printf.sprintf "INST_IDIV %s"             (prettyprint_token_info pos)
  | Parser.INST_IADD(pos)             -> Printf.sprintf "INST_IADD %s"             (prettyprint_token_info pos)
  | Parser.INST_IINC(pos)             -> Printf.sprintf "INST_IINC %s"             (prettyprint_token_info pos)
  | Parser.INST_GOTO(pos)             -> Printf.sprintf "INST_GOTO %s"             (prettyprint_token_info pos)
  | Parser.INST_IFEQ(pos)             -> Printf.sprintf "INST_IFEQ %s"             (prettyprint_token_info pos)
  | Parser.INST_IFNE(pos)             -> Printf.sprintf "INST_IFNE %s"             (prettyprint_token_info pos)
  | Parser.INST_IF_ACMPEQ(pos)        -> Printf.sprintf "INST_IF_ACMPEQ %s"        (prettyprint_token_info pos)
  | Parser.INST_IF_ACMPNE(pos)        -> Printf.sprintf "INST_IF_ACMPNE %s"        (prettyprint_token_info pos)
  | Parser.INST_IFNULL(pos)           -> Printf.sprintf "INST_IFNULL %s"           (prettyprint_token_info pos)
  | Parser.INST_IFNONNULL(pos)        -> Printf.sprintf "INST_IFNONNULL %s"        (prettyprint_token_info pos)
  | Parser.INST_IF_ICMPEQ(pos)        -> Printf.sprintf "INST_IF_ICMPEQ %s"        (prettyprint_token_info pos)
  | Parser.INST_IF_ICMPGT(pos)        -> Printf.sprintf "INST_IF_ICMPGT %s"        (prettyprint_token_info pos)
  | Parser.INST_IF_ICMPLT(pos)        -> Printf.sprintf "INST_IF_ICMPLT %s"        (prettyprint_token_info pos)
  | Parser.INST_IF_ICMPLE(pos)        -> Printf.sprintf "INST_IF_ICMPLE %s"        (prettyprint_token_info pos)
  | Parser.INST_IF_ICMPGE(pos)        -> Printf.sprintf "INST_IF_ICMPGE %s"        (prettyprint_token_info pos)
  | Parser.INST_IF_ICMPNE(pos)        -> Printf.sprintf "INST_IF_ICMPNE %s"        (prettyprint_token_info pos)
  | Parser.INST_IRETURN(pos)          -> Printf.sprintf "INST_IRETURN %s"          (prettyprint_token_info pos)
  | Parser.INST_ARETURN(pos)          -> Printf.sprintf "INST_ARETURN %s"          (prettyprint_token_info pos)
  | Parser.INST_RETURN(pos)           -> Printf.sprintf "INST_RETURN %s"           (prettyprint_token_info pos)
  | Parser.INST_ALOAD(pos)            -> Printf.sprintf "INST_ALOAD %s"            (prettyprint_token_info pos)
  | Parser.INST_ASTORE(pos)           -> Printf.sprintf "INST_ASTORE %s"           (prettyprint_token_info pos)
  | Parser.INST_ILOAD(pos)            -> Printf.sprintf "INST_ILOAD %s"            (prettyprint_token_info pos)
  | Parser.INST_ISTORE(pos)           -> Printf.sprintf "INST_ISTORE %s"           (prettyprint_token_info pos)
  | Parser.INST_DUP(pos)              -> Printf.sprintf "INST_DUP %s"              (prettyprint_token_info pos)
  | Parser.INST_POP(pos)              -> Printf.sprintf "INST_POP %s"              (prettyprint_token_info pos)
  | Parser.INST_SWAP(pos)             -> Printf.sprintf "INST_SWAP %s"             (prettyprint_token_info pos)
  | Parser.INST_LDC(pos)              -> Printf.sprintf "INST_LDC %s"              (prettyprint_token_info pos)
  | Parser.INST_ACONST_NULL(pos)      -> Printf.sprintf "INST_ACONST_NULL %s"      (prettyprint_token_info pos)
  | Parser.INST_GETFIELD(pos)         -> Printf.sprintf "INST_GETFIELD %s"         (prettyprint_token_info pos)
  | Parser.INST_PUTFIELD(pos)         -> Printf.sprintf "INST_PUTFIELD %s"         (prettyprint_token_info pos)
  | Parser.INST_INVOKEVIRTUAL(pos)    -> Printf.sprintf "INST_INVOKEVIRTUAL %s"    (prettyprint_token_info pos)
  | Parser.INST_INVOKENONVIRTUAL(pos) -> Printf.sprintf "INST_INVOKENONVIRTUAL %s" (prettyprint_token_info pos)
  (* keywords *)
  | Parser.KEYWORD_PATTERN(pos)       -> Printf.sprintf "KEYWORD_PATTERN[.pattern] %s"  (prettyprint_token_info pos)
  | Parser.KEYWORD_PRECONDITION(pos)  -> Printf.sprintf "KEYWORD_PRECONDITION[.pre] %s" (prettyprint_token_info pos)
  | Parser.KEYWORD_DEFINITION(pos)    -> Printf.sprintf "KEYWORD_DEFINITION[.def] %s"   (prettyprint_token_info pos)
  | Parser.KEYWORD_END(pos)           -> Printf.sprintf "KEYWORD_END[.end] %s"          (prettyprint_token_info pos)
  | Parser.KEYWORD_ARROW(pos)         -> Printf.sprintf "KEYWORD_ARROW[=>] %s"          (prettyprint_token_info pos)
  | Parser.KEYWORD_SCHEMA(pos)        -> Printf.sprintf "KEYWORD_SCHEMA[.schema] %s"    (prettyprint_token_info pos)
  | Parser.KEYWORD_EXTERN(pos)        -> Printf.sprintf "KEYWORD_EXTERN[extern] %s"     (prettyprint_token_info pos)
  (* literals *)
  | Parser.STRING(pos, value)         -> Printf.sprintf "STRING[%s] %s"      value (prettyprint_token_info pos)
  | Parser.METHOD_SPEC(pos, value)    -> Printf.sprintf "METHOD_SPEC[%s] %s" value (prettyprint_token_info pos)
  | Parser.NAME(pos, value)           -> Printf.sprintf "NAME[%s] %s"        value (prettyprint_token_info pos)
  | Parser.NUMERIC(pos, value)        -> Printf.sprintf "NUMERIC[%d] %s"     value (prettyprint_token_info pos)
  (* operators *)
  | Parser.OP_PARAM_LEFT(pos)    -> Printf.sprintf "OP_PARAM_LEFT %s"    (prettyprint_token_info pos)
  | Parser.OP_PARAM_RIGHT(pos)   -> Printf.sprintf "OP_PARAM_RIGHT %s"   (prettyprint_token_info pos)
  | Parser.OP_SQUARE_LEFT(pos)   -> Printf.sprintf "OP_SQUARE_LEFT %s"   (prettyprint_token_info pos)
  | Parser.OP_SQUARE_RIGHT(pos)  -> Printf.sprintf "OP_SQUARE_RIGHT %s"  (prettyprint_token_info pos)
  | Parser.OP_CURLY_LEFT(pos)    -> Printf.sprintf "OP_CURLY_LEFT %s"    (prettyprint_token_info pos)
  | Parser.OP_CURLY_RIGHT(pos)   -> Printf.sprintf "OP_CURLY_RIGHT %s"   (prettyprint_token_info pos)
  | Parser.OP_ANGULAR_LEFT(pos)  -> Printf.sprintf "OP_ANGULAR_LEFT %s"  (prettyprint_token_info pos)
  | Parser.OP_ANGULAR_RIGHT(pos) -> Printf.sprintf "OP_ANGULAR_RIGHT %s" (prettyprint_token_info pos)
  | Parser.OP_ASSIGN(pos)        -> Printf.sprintf "OP_ASSIGN %s"        (prettyprint_token_info pos)
  | Parser.OP_PLUS(pos)          -> Printf.sprintf "OP_PLUS %s"          (prettyprint_token_info pos)
  | Parser.OP_DASH(pos)          -> Printf.sprintf "OP_DASH %s"          (prettyprint_token_info pos)
  | Parser.OP_STAR(pos)          -> Printf.sprintf "OP_STAR %s"          (prettyprint_token_info pos)
  | Parser.OP_SLASH(pos)         -> Printf.sprintf "OP_SLASH %s"         (prettyprint_token_info pos)
  | Parser.OP_REM(pos)           -> Printf.sprintf "OP_REM %s"           (prettyprint_token_info pos)
  | Parser.OP_OR(pos)            -> Printf.sprintf "OP_OR %s"            (prettyprint_token_info pos)
  | Parser.OP_AND(pos)           -> Printf.sprintf "OP_AND %s"           (prettyprint_token_info pos)
  | Parser.OP_EQ(pos)            -> Printf.sprintf "OP_EQ %s"            (prettyprint_token_info pos)
  | Parser.OP_NE(pos)            -> Printf.sprintf "OP_NE %s"            (prettyprint_token_info pos)
  | Parser.OP_GT(pos)            -> Printf.sprintf "OP_GT %s"            (prettyprint_token_info pos)
  | Parser.OP_LT(pos)            -> Printf.sprintf "OP_LT %s"            (prettyprint_token_info pos)
  | Parser.OP_GE(pos)            -> Printf.sprintf "OP_GE %s"            (prettyprint_token_info pos)
  | Parser.OP_LE(pos)            -> Printf.sprintf "OP_LE %s"            (prettyprint_token_info pos)
  | Parser.OP_COMMA(pos)         -> Printf.sprintf "OP_COMMA %s"         (prettyprint_token_info pos)
  | Parser.OP_COLON(pos)         -> Printf.sprintf "OP_COLON %s"         (prettyprint_token_info pos)
  (* special symbols *)                       
  | Parser.EOF(pos) -> Printf.sprintf "EOF %s" (prettyprint_token_info pos)
  | Parser.EOL(pos) -> Printf.sprintf "EOL %s" (prettyprint_token_info pos)
                     
let prettyprint_name = fun (name :string list) ->
  let buffer = Buffer.create 32 in
  let rec prettyprint_name' = fun (name' :string list) ->
    match name' with
    | []           -> Buffer.contents buffer
    | head :: []   -> Buffer.add_string buffer head ;
                      Buffer.contents buffer
    | head :: tail -> Buffer.add_string buffer head ;
                      Buffer.add_char buffer '/' ;
                      prettyprint_name' tail
  in
  prettyprint_name' name

let prettyprint_name_natural = fun (name :string list) ->
    let buffer = Buffer.create 32 in
  let rec prettyprint_name_natural' = fun (name' :string list) ->
    match name' with
    | []           -> Buffer.contents buffer
    | head :: []   -> Buffer.add_string buffer head ;
                      Buffer.contents buffer
    | head :: tail -> Buffer.add_string buffer head ;
                      Buffer.add_char buffer '.' ;
                      prettyprint_name_natural' tail
  in
  prettyprint_name_natural' name
                     
let rec prettyprint_type_spec = fun (t :AST.type_spec) ->
  match t with
  | AST.Boolean               -> "Z"
  | AST.Byte                  -> "B"
  | AST.Char                  -> "C"
  | AST.Short                 -> "S"
  | AST.Integer               -> "I"
  | AST.Long                  -> "J"
  | AST.Float                 -> "F"
  | AST.Double                -> "D"
  | AST.Void                  -> "V"
  | AST.ClassSpec(class_name) -> let buffer = Buffer.create 32 in
                                 Buffer.add_char buffer 'L' ;
                                 Buffer.add_string buffer (prettyprint_name class_name) ;
                                 Buffer.add_char buffer ';' ;
                                 Buffer.contents buffer
  | AST.Array(t)              -> let buffer = Buffer.create 32 in
                                 Buffer.add_char buffer '[' ;
                                 Buffer.add_string buffer (prettyprint_type_spec t) ;
                                 Buffer.contents buffer

let prettyprint_type_specs = fun (t :AST.type_spec list) ->
  let buffer = Buffer.create 32 in
  let rec prettyprint_type_specs' = fun (t' :AST.type_spec list) ->
    match t' with
    | []           -> ()
    | head :: tail -> Buffer.add_string buffer (prettyprint_type_spec head) ;
                      prettyprint_type_specs' tail
  in
  prettyprint_type_specs' t ;
  Buffer.contents buffer

let rec prettyprint_type_spec_natural = fun (t :AST.type_spec) ->
  match t with
  | AST.Boolean               -> "boolean"
  | AST.Byte                  -> "byte"
  | AST.Char                  -> "char"
  | AST.Short                 -> "short"
  | AST.Integer               -> "int"
  | AST.Long                  -> "long"
  | AST.Float                 -> "float"
  | AST.Double                -> "double"
  | AST.Void                  -> "void"
  | AST.ClassSpec(class_name) -> let buffer = Buffer.create 32 in
                                 Buffer.add_string buffer (prettyprint_name_natural class_name) ;
                                 Buffer.contents buffer
  | AST.Array(t)              -> let buffer = Buffer.create 32 in
                                 Buffer.add_string buffer (prettyprint_type_spec_natural t) ;
                                 Buffer.add_string buffer "[]" ;
                                 Buffer.contents buffer

let prettyprint_type_specs_natural = fun (t :AST.type_spec list) ->
  let buffer = Buffer.create 32 in
  let rec prettyprint_type_specs_natural' = fun (t' :AST.type_spec list) ->
    match t' with
    | [] -> ()
    | head :: []   -> Buffer.add_string buffer (prettyprint_type_spec_natural head) ;
                      ()
    | head :: tail -> Buffer.add_string buffer (prettyprint_type_spec_natural head) ;
                      Buffer.add_string buffer ", " ;
                      prettyprint_type_specs_natural' tail
  in
  prettyprint_type_specs_natural' t ;
  Buffer.contents buffer

let prettyprint_method_spec = fun (t :AST.method_spec) ->
  let buffer = Buffer.create 32 in
  let (class_name, method_name, param_type, ret_type) = t in
  Buffer.add_string buffer (prettyprint_name class_name) ;
  Buffer.add_char buffer '/' ;
  Buffer.add_string buffer method_name ;
  Buffer.add_char buffer '(' ;
  Buffer.add_string buffer (prettyprint_type_specs param_type) ;
  Buffer.add_char buffer ')' ;
  Buffer.add_string buffer (prettyprint_type_spec ret_type);
  Buffer.contents buffer

let prettyprint_method_spec_natural = fun (t :AST.method_spec) ->
  let buffer = Buffer.create 32 in
  let (class_name, method_name, param_type, ret_type) = t in
  Buffer.add_string buffer (prettyprint_type_spec_natural ret_type) ;
  Buffer.add_char buffer ' ' ;
  Buffer.add_string buffer (prettyprint_name_natural class_name) ;
  Buffer.add_char buffer '.' ;
  Buffer.add_string buffer method_name ;
  Buffer.add_char buffer '(' ;
  Buffer.add_string buffer (prettyprint_type_specs_natural param_type) ;
  Buffer.add_char buffer ')';
  Buffer.contents buffer

let prettyprint_operand = fun (t :AST.operand) ->
  match t with
  | AST.CaptureName({value = v; _}) -> Printf.sprintf "[%s]" v
  | AST.Name({value = v; _})        -> prettyprint_name v
  | AST.String({value = v; _})      -> Printf.sprintf "\"%s\"" v
  | AST.Numeric({value = v; _})     -> Printf.sprintf "%d" v
  | AST.MethodSpec({value = v; _})  -> prettyprint_method_spec v

let prettyprint_operand_natural = fun (t :AST.operand) ->
  match t with
  | AST.CaptureName({value = v; _}) -> Printf.sprintf "[%s]" v
  | AST.Name({value = v; _})        -> prettyprint_name_natural v
  | AST.String({value = v; _})      -> Printf.sprintf "\"%s\"" v
  | AST.Numeric({value = v; _})     -> Printf.sprintf "%d" v
  | AST.MethodSpec({value = v; _})  -> prettyprint_method_spec_natural v

let rec prettyprint_expression = fun (t :AST.expression) ->
  let prettyprint_binary = fun (lhs :AST.expression) (rhs :AST.expression) (op :string) ->
    let lhs_prettyprint = prettyprint_expression lhs in
    let rhs_prettyprint = prettyprint_expression rhs in
    Printf.sprintf "(%s%s%s)" lhs_prettyprint op rhs_prettyprint
  in
  match t with
  | AST.AddExpr({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "+"
  | AST.SubExpr({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "-"
  | AST.MulExpr({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "*"
  | AST.DivExpr({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "/"
  | AST.RemExpr({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "%"
  | AST.AndExpr({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "&&"
  | AST.OrExpr ({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "||"
  | AST.EQExpr ({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "=="
  | AST.NEExpr ({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "!="
  | AST.GTExpr ({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs ">"
  | AST.LTExpr ({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "<"
  | AST.GEExpr ({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs ">="
  | AST.LEExpr ({lhs = lhs; rhs = rhs; _}) -> prettyprint_binary lhs rhs "<="
  | AST.OperandExpr({operand = operand; _})-> prettyprint_operand operand
  | AST.InvokeExpr({target = target; operands = operands; _}) ->
     let buffer = Buffer.create 32 in
     Buffer.add_string buffer target ;
     Buffer.add_char buffer '(' ;
     let rec prettyprint_operands = fun (operands :AST.expression list) ->
       match operands with
       | head :: []   -> Buffer.add_string buffer (prettyprint_expression head)
       | head :: tail -> Buffer.add_string buffer (prettyprint_expression head) ;
                         Buffer.add_string buffer ", " ;
                         prettyprint_operands tail
       | []           -> ()
     in
     prettyprint_operands operands ;
     Buffer.add_char buffer ')' ;
     Buffer.contents buffer
     
let prettyprint_instruction = fun (instruction :AST.instruction) ->
  match instruction with
  | AST.InstNop(_)                           -> "nop"
  | AST.InstI2c(_)                           -> "i2c"
  | AST.InstNew(_)                           -> "new"
  | AST.InstInstanceof(_)                    -> "instanceof"
  | AST.InstCheckcast(_)                     -> "checkcast"
  | AST.InstImul(_)                          -> "imul"
  | AST.InstIneg(_)                          -> "ineg"
  | AST.InstIrem(_)                          -> "irem"
  | AST.InstIsub(_)                          -> "isub"
  | AST.InstIdiv(_)                          -> "idiv"
  | AST.InstIadd(_)                          -> "iadd"
  | AST.InstIinc(_)                          -> "iinc"
  | AST.InstGoto(_)                          -> "goto"
  | AST.InstIfeq(_)                          -> "ifeq"
  | AST.InstIfne(_)                          -> "ifne"
  | AST.InstIf_acmpeq(_)                     -> "if_acmpeq"
  | AST.InstIf_acmpne(_)                     -> "if_acmpne"
  | AST.InstIfnull(_)                        -> "ifnull"
  | AST.InstIfnonnull(_)                     -> "ifnonnull"
  | AST.InstIf_icmpeq(_)                     -> "if_icmpeq"
  | AST.InstIf_icmpgt(_)                     -> "if_icmpgt"
  | AST.InstIf_icmplt(_)                     -> "if_icmplt"
  | AST.InstIf_icmple(_)                     -> "if_icmple"
  | AST.InstIf_icmpge(_)                     -> "if_icmpge"
  | AST.InstIf_icmpne(_)                     -> "if_icmpne"
  | AST.InstIreturn(_)                       -> "ireturn"
  | AST.InstAreturn(_)                       -> "areturn"
  | AST.InstReturn(_)                        -> "return"
  | AST.InstAload(_)                         -> "aload"
  | AST.InstAstore(_)                        -> "astore"
  | AST.InstIload(_)                         -> "iload"
  | AST.InstIstore(_)                        -> "istore"
  | AST.InstDup(_)                           -> "dup"
  | AST.InstPop(_)                           -> "pop"
  | AST.InstSwap(_)                          -> "swap"
  | AST.InstLdc(_)                           -> "ldc"
  | AST.InstAconst_null(_)                   -> "aconst_null"
  | AST.InstGetfield(_)                      -> "getfield"
  | AST.InstPutfield(_)                      -> "putfield"
  | AST.InstInvokevirtual(_)                 -> "invokevirtual"
  | AST.InstInvokenonvirtual(_)              -> "invokenonvirtual"
  | AST.InstUserDefined({value = value ; _}) -> value

let prettyprint_statement = fun (statement :AST.statement) ->
  match statement with
  | AST.LiteralLabel {name = name; _}        -> Printf.sprintf "%s :" name
  | AST.CaptureLabel {name = name; _}        -> Printf.sprintf "[%s] :" name
  | AST.BlockOneStmt {name = name; _}        -> Printf.sprintf "< [%s] >" name
  | AST.BlockOneOrMoreStmt {name = name; _}  -> Printf.sprintf "< [%s]+ >" name
  | AST.BlockZeroOrMoreStmt {name = name; _} -> Printf.sprintf "< [%s]* >" name
  | AST.Instruction {target = target; operands = operands; _} ->
     let buffer = Buffer.create 32 in
     Buffer.add_string buffer (prettyprint_instruction target) ;
     Buffer.add_char buffer ' ' ;
     let rec prettyprint_operand = fun (operands :AST.expression list) ->
       let decorate_expression = fun (expression :AST.expression) ->
         let prettyprint_result = prettyprint_expression expression in
         if AST.is_expression_extended expression then
           Printf.sprintf "{%s}" prettyprint_result
         else
           prettyprint_result
       in
       match operands with
       | []           -> ()
       | head :: []   -> Buffer.add_string buffer (decorate_expression head)
       | head :: tail -> Buffer.add_string buffer (decorate_expression head) ;
                         Buffer.add_char buffer ' ' ;
                         prettyprint_operand tail
     in
     prettyprint_operand operands ;
     Buffer.contents buffer

let prettyprint_precondition = fun (expression :AST.expression) ->
  Printf.sprintf ".pre %s" (prettyprint_expression expression)

let prettyprint_definition = fun (definition :AST.definition) ->
  let name = definition.AST.name in
  let alternatives = definition.AST.alternatives in
  let buffer = Buffer.create 32 in
  let rec write_alternatives_to_buffer = fun (alternatives :AST.instruction list) ->
    match alternatives with
    | []           -> ()
    | head :: []   -> Buffer.add_string buffer (prettyprint_instruction head)
    | head :: tail -> Buffer.add_string buffer (prettyprint_instruction head) ;
                      Buffer.add_string buffer ", " ;
                      write_alternatives_to_buffer tail
  in
  write_alternatives_to_buffer alternatives ;
  let alternatives_string = Buffer.contents buffer in
  if String.length alternatives_string <= 70 then 
    Printf.sprintf ".def %s = {%s}" name alternatives_string
  else
    let split_regex = Str.regexp ", " in
    let tokens = Str.split split_regex alternatives_string in
    let rec print_multipleline = fun (buffer :Buffer.t) (tokens :string list) (len :int)->
      match tokens with
      | []           -> let content = Buffer.contents buffer in
                        if len <> 0 then
                          [ content ]
                        else
                          []
      | head :: tail -> if (len + String.length head) <= 70 then
                          if len == 0 then
                            (Buffer.add_string buffer head ;
                             print_multipleline buffer tail (String.length head))
                          else
                            (Buffer.add_string buffer ", " ;
                             Buffer.add_string buffer head ;
                             print_multipleline buffer tail (len + 2 + String.length head))
                        else
                          let single_line = Buffer.contents buffer in
                          let new_buffer = Buffer.create 32 in
                          Buffer.add_string new_buffer head ;
                          single_line :: (print_multipleline new_buffer tail (String.length head))
    in
    let lines = print_multipleline (Buffer.create 32) tokens 0 in
    let buffer = Buffer.create 32 in
    Buffer.add_string buffer (Printf.sprintf ".def %s = {\n" name) ;
    let rec print_lines = fun (lines :string list) ->
      match lines with
      | []           -> ()
      | [ line ]     -> Buffer.add_string buffer (Printf.sprintf "    %s\n" line)
      | head :: tail -> Buffer.add_string buffer (Printf.sprintf "    %s,\n" head) ;
                        print_lines tail
    in
    print_lines lines ;
    Buffer.add_string buffer "  }" ;
    Buffer.contents buffer
  
let prettyprint_pattern = fun (pattern :AST.pattern) ->
  let buffer = Buffer.create 32 in
  Buffer.add_string buffer (Printf.sprintf ".pattern %s\n" pattern.AST.name) ;
  List.iter
    (fun (definition :AST.definition) ->
      let single_line =
        Printf.sprintf "  %s\n" (prettyprint_definition definition)
      in Buffer.add_string buffer single_line)
    pattern.AST.definitions ;
  List.iter
    (fun (precondition :AST.expression) ->
      let single_line =
        Printf.sprintf "  %s\n" (prettyprint_precondition precondition)
      in Buffer.add_string buffer single_line)
    pattern.AST.preconditions ;
  List.iter
    (fun (statement :AST.statement) ->
      let single_line =
        Printf.sprintf "  %s\n" (prettyprint_statement statement)
      in Buffer.add_string buffer single_line)
    pattern.AST.capture_patterns ;
  Buffer.add_string buffer "  =>\n" ;
  List.iter
    (fun (statement :AST.statement) ->
      let single_line =
        Printf.sprintf "  %s\n" (prettyprint_statement statement)
      in Buffer.add_string buffer single_line)
    pattern.AST.rewrite_patterns ;
  Buffer.add_string buffer ".end" ;
  Buffer.contents buffer

let prettyprint_schema_element = fun (schema_element :AST.schema_element) ->
  match schema_element with
  | AST.InternalPattern {name = name; _} -> name
  | AST.ExternalPattern {name = name; _} -> Printf.sprintf ".extern %s" name

let prettyprint_schema = fun (schema :AST.schema) ->
  let buffer = Buffer.create 32 in
  Buffer.add_string buffer ".schema\n" ;
  List.iter
    (fun (schema_element :AST.schema_element) ->
      let single_line =
        Printf.sprintf "  %s\n" (prettyprint_schema_element schema_element)
      in Buffer.add_string buffer single_line)
    schema.AST.schema ;
  Buffer.add_string buffer ".end" ;
  Buffer.contents buffer

let prettyprint_compilation_unit = fun (compilation_unit :AST.compilation_unit) ->
  let buffer = Buffer.create 32 in
  List.iter
    (fun (schema :AST.schema) ->
      let schema_string = prettyprint_schema schema in
      Buffer.add_string buffer schema_string ;
      Buffer.add_char buffer '\n' ;
      Buffer.add_char buffer '\n')
    compilation_unit.AST.schemas ;
  List.iter
    (fun (pattern :AST.pattern) ->
      let pattern_string = prettyprint_pattern pattern in
      Buffer.add_string buffer pattern_string ;
      Buffer.add_char buffer '\n' ;
      Buffer.add_char buffer '\n')
    compilation_unit.AST.patterns ;
  Buffer.contents buffer

(* support for xterm coloring *)
module PrettyPrintColorSchema = struct
  type coloring_schema = {
      keyword     : string -> string ;
      variable    : string -> string ;
      instruction : string -> string ;
      method_spec : string -> string ;
      string      : string -> string ;
      numeric     : string -> string 
    }
      
  let default_color_schema =
    { keyword     = (fun str -> Printf.sprintf "\027[38;5;148m%s\027[0m" str) ;
      variable    = (fun str -> Printf.sprintf "\027[38;5;202m%s\027[0m" str) ;
      instruction = (fun str -> Printf.sprintf "\027[38;5;177m%s\027[0m"  str) ;
      method_spec = (fun str -> Printf.sprintf "\027[38;5;220m%s\027[0m" str) ;
      string      = (fun str -> Printf.sprintf "\027[38;5;200m%s\027[0m" str) ;
      numeric     = (fun str -> Printf.sprintf "\027[38;5;45m%s\027[0m"  str) 
    }

  let empty_color_schema =
    { keyword     = (fun str -> str) ;
      variable    = (fun str -> str) ;
      instruction = (fun str -> str) ;
      method_spec = (fun str -> str) ;
      string      = (fun str -> str) ;
      numeric     = (fun str -> str)
    }
end                             

let prettyprint_color_schema = ref PrettyPrintColorSchema.default_color_schema

let set_prettyprint_color_schema = fun (schema :PrettyPrintColorSchema.coloring_schema) ->
  prettyprint_color_schema := schema ;
  ()

let prettyprint_name_color = fun (name :string list) ->
  prettyprint_name name

let prettyprint_type_spec_color = fun (t :AST.type_spec) ->
  prettyprint_type_spec t

let prettyprint_type_specs_color = fun (t :AST.type_spec list) ->
  prettyprint_type_specs t

let prettyprint_method_spec_color = fun (t :AST.method_spec) ->
  let content = prettyprint_method_spec t in
  let method_spec_decorator =
    (!prettyprint_color_schema).PrettyPrintColorSchema.method_spec
  in method_spec_decorator content

let prettyprint_operand_color = fun (t :AST.operand) ->
  match t with
  | AST.CaptureName(_) ->
     let variable_decorator =
       (!prettyprint_color_schema).PrettyPrintColorSchema.variable
     in variable_decorator (prettyprint_operand t)
  | AST.Name(_) -> prettyprint_operand t
  | AST.String(_) ->
     let string_decorator =
       (!prettyprint_color_schema).PrettyPrintColorSchema.string
     in string_decorator (prettyprint_operand t)
  | AST.Numeric(_) ->
     let numeric_decorator =
       (!prettyprint_color_schema).PrettyPrintColorSchema.numeric
     in numeric_decorator (prettyprint_operand t)
  | AST.MethodSpec(_) ->
     let method_spec_decorator =
       (!prettyprint_color_schema).PrettyPrintColorSchema.method_spec
     in method_spec_decorator (prettyprint_operand t)

let rec prettyprint_expression_color = fun (t :AST.expression) ->
  let prettyprint_binary_expression = fun (lhs :AST.expression) (rhs :AST.expression) (op :string) ->
      let lhs_prettyprint = prettyprint_expression_color lhs in
      let rhs_prettyprint = prettyprint_expression_color rhs in
      Printf.sprintf "(%s%s%s)" lhs_prettyprint op rhs_prettyprint
  in
  match t with
  | AST.AddExpr{lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "+"
  | AST.SubExpr{lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "-"
  | AST.MulExpr{lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "*"
  | AST.DivExpr{lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "/"
  | AST.RemExpr{lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "%"
  | AST.AndExpr{lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "&&"
  | AST.OrExpr {lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "||"
  | AST.EQExpr {lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "=="
  | AST.NEExpr {lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "!="
  | AST.GTExpr {lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs ">"
  | AST.LTExpr {lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "<"
  | AST.GEExpr {lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs ">="
  | AST.LEExpr {lhs = lhs; rhs = rhs; _} -> prettyprint_binary_expression lhs rhs "<="
  | AST.OperandExpr {operand = operand; _} -> prettyprint_operand_color operand
  | AST.InvokeExpr {target = target; operands = operands; _} ->
     let buffer = Buffer.create 32 in
     Buffer.add_string buffer target ;
     Buffer.add_char buffer '(' ;
     let rec prettyprint_operands = fun (operands :AST.expression list) ->
       match operands with
       | head :: []   -> Buffer.add_string buffer (prettyprint_expression_color head)
       | head :: tail -> Buffer.add_string buffer (prettyprint_expression_color head) ;
                         Buffer.add_string buffer ", " ;
                         prettyprint_operands tail
       | []           -> ()
     in
     prettyprint_operands operands ;
     Buffer.add_char buffer ')' ;
     Buffer.contents buffer

let prettyprint_instruction_color = fun (instruction :AST.instruction) ->
  match instruction with
  | AST.InstUserDefined(_) -> prettyprint_instruction instruction
  | _ ->
     let instruction_decorator =
       (!prettyprint_color_schema).PrettyPrintColorSchema.instruction
     in instruction_decorator (prettyprint_instruction instruction)

let prettyprint_statement_color = fun (statement :AST.statement) ->
  let decorate_name_as_variable = fun (name :string) ->
    let variable_decorator =
      (!prettyprint_color_schema).PrettyPrintColorSchema.variable
    in variable_decorator (Printf.sprintf "[%s]" name)
  in
  match statement with
  | AST.LiteralLabel(_) -> prettyprint_statement statement
  | AST.CaptureLabel{name = name; _} ->
     Printf.sprintf "%s :"    (decorate_name_as_variable name)
  | AST.BlockOneStmt{name = name; _} ->
     Printf.sprintf "< %s >"  (decorate_name_as_variable name)
  | AST.BlockOneOrMoreStmt{name = name; _} ->
     Printf.sprintf "< %s+ >" (decorate_name_as_variable name)
  | AST.BlockZeroOrMoreStmt{name = name; _} ->
     Printf.sprintf "< %s* >" (decorate_name_as_variable name)
  | AST.Instruction{target = target; operands = operands; _} ->
     let buffer = Buffer.create 32 in
     Buffer.add_string buffer (prettyprint_instruction_color target) ;
     Buffer.add_char buffer ' ' ;
     let rec prettyprint_operand_color = fun (operands :AST.expression list) ->
       let decorate_expression = fun (expression :AST.expression) ->
         let prettyprint_result = prettyprint_expression_color expression in
         if AST.is_expression_extended expression then
           Printf.sprintf "{%s}" prettyprint_result
         else
           prettyprint_result
       in
       match operands with
       | []           -> ()
       | head :: []   -> Buffer.add_string buffer (decorate_expression head)
       | head :: tail -> Buffer.add_string buffer (decorate_expression head) ;
                         Buffer.add_char buffer ' ' ;
                         prettyprint_operand_color tail
     in
     prettyprint_operand_color operands ;
     Buffer.contents buffer 

let prettyprint_precondition_color = fun (expression :AST.expression) ->
  let buffer = Buffer.create 32 in
  let keyword_decorator = (!prettyprint_color_schema).PrettyPrintColorSchema.keyword in
  Buffer.add_string buffer (keyword_decorator ".pre") ;
  Buffer.add_char buffer ' ' ;
  Buffer.add_string buffer (prettyprint_expression_color expression) ;
  Buffer.contents buffer 

let prettyprint_definition_color = fun (definition :AST.definition) ->
  let line_break_limit = 150 in
  let name = definition.AST.name in
  let alternatives = definition.AST.alternatives in
  let buffer = Buffer.create 32 in
  let keyword_decorator = (!prettyprint_color_schema).PrettyPrintColorSchema.keyword in
  let rec write_alternatives_to_buffer = fun (alternatives :AST.instruction list) ->
    match alternatives with
    | []           -> ()
    | head :: []   -> Buffer.add_string buffer (prettyprint_instruction_color head)
    | head :: tail -> Buffer.add_string buffer (prettyprint_instruction_color head) ;
                      Buffer.add_string buffer ", " ;
                      write_alternatives_to_buffer tail
  in
  write_alternatives_to_buffer alternatives ;
  let alternatives_string = Buffer.contents buffer in
  if String.length alternatives_string <= line_break_limit then
    let out_buffer = Buffer.create 32 in
    Buffer.add_string out_buffer (keyword_decorator ".def") ;
    Buffer.add_string out_buffer (Printf.sprintf " %s = {%s}" name alternatives_string) ;
    Buffer.contents out_buffer
  else
    let split_regex = Str.regexp ", " in
    let tokens = Str.split split_regex alternatives_string in
    let rec print_multipleline = fun (buffer :Buffer.t) (tokens :string list) (len :int)->
      match tokens with
      | []           -> let content = Buffer.contents buffer in
                        if len <> 0 then
                          [ content ]
                        else
                          []
      | head :: tail -> if (len + String.length head) <= line_break_limit then
                          if len == 0 then
                            (Buffer.add_string buffer head ;
                             print_multipleline buffer tail (String.length head))
                          else
                            (Buffer.add_string buffer ", " ;
                             Buffer.add_string buffer head ;
                             print_multipleline buffer tail (len + 2 + String.length head))
                        else
                          let single_line = Buffer.contents buffer in
                          let new_buffer = Buffer.create 32 in
                          Buffer.add_string new_buffer head ;
                          single_line :: (print_multipleline new_buffer tail (String.length head))
    in
    let lines = print_multipleline (Buffer.create 32) tokens 0 in
    let buffer = Buffer.create 32 in
    Buffer.add_string buffer (keyword_decorator ".def") ;
    Buffer.add_string buffer (Printf.sprintf " %s = {\n" name) ;
    let rec print_lines = fun (lines :string list) ->
      match lines with
      | []           -> ()
      | [ line ]     -> Buffer.add_string buffer (Printf.sprintf "    %s\n" line)
      | head :: tail -> Buffer.add_string buffer (Printf.sprintf "    %s,\n" head) ;
                        print_lines tail
    in
    print_lines lines ;
    Buffer.add_string buffer "  }" ;
    Buffer.contents buffer

let prettyprint_pattern_color = fun (pattern :AST.pattern) ->
  let keyword_decorator =
    (!prettyprint_color_schema).PrettyPrintColorSchema.keyword
  in
  let buffer = Buffer.create 32 in
  Buffer.add_string buffer (keyword_decorator ".pattern") ;
  Buffer.add_string buffer (Printf.sprintf " %s\n" pattern.AST.name) ;
  List.iter
    (fun (definition :AST.definition) ->
      let single_line =
        Printf.sprintf "  %s\n" (prettyprint_definition_color definition)
      in Buffer.add_string buffer single_line)
    pattern.AST.definitions ;
  List.iter
    (fun (precondition :AST.expression) ->
      let single_line =
        Printf.sprintf "  %s\n" (prettyprint_precondition_color precondition)
      in Buffer.add_string buffer single_line)
    pattern.AST.preconditions ;
  List.iter
    (fun (statement :AST.statement) ->
      let single_line =
        Printf.sprintf "  %s\n" (prettyprint_statement_color statement)
      in Buffer.add_string buffer single_line)
    pattern.AST.capture_patterns ;
  Buffer.add_string buffer "  " ;
  Buffer.add_string buffer (keyword_decorator "=>") ; 
  Buffer.add_char buffer '\n' ;
  List.iter
    (fun (statement :AST.statement) ->
      let single_line =
        Printf.sprintf "  %s\n" (prettyprint_statement_color statement)
      in Buffer.add_string buffer single_line)
    pattern.AST.rewrite_patterns ;
  Buffer.add_string buffer (keyword_decorator ".end") ;
  Buffer.contents buffer

let prettyprint_schema_element_color = fun (schema_element :AST.schema_element) ->
  let keyword_decorator =
    (!prettyprint_color_schema).PrettyPrintColorSchema.keyword
  in
  match schema_element with
  | AST.InternalPattern {name = name; _} -> name
  | AST.ExternalPattern {name = name; _} ->     
     Printf.sprintf "%s %s" (keyword_decorator ".extern") name

let prettyprint_schema_color = fun (schema :AST.schema) ->
  let keyword_decorator =
    (!prettyprint_color_schema).PrettyPrintColorSchema.keyword
  in
  let buffer = Buffer.create 32 in
  Buffer.add_string buffer (Printf.sprintf "%s\n" (keyword_decorator ".schema"));
  List.iter
    (fun (schema_element :AST.schema_element) ->
      let single_line =
        Printf.sprintf "  %s\n" (prettyprint_schema_element_color schema_element)
      in Buffer.add_string buffer single_line)
    schema.AST.schema ;
  Buffer.add_string buffer (Printf.sprintf "%s" (keyword_decorator ".end")) ;
  Buffer.contents buffer

let prettyprint_compilation_unit_color = fun (compilation_unit :AST.compilation_unit) ->
  let buffer = Buffer.create 32 in
  List.iter
    (fun (schema :AST.schema) ->
      let schema_string = prettyprint_schema_color schema in
      Buffer.add_string buffer schema_string ;
      Buffer.add_char buffer '\n' ;
      Buffer.add_char buffer '\n')
    compilation_unit.AST.schemas ;
  List.iter
    (fun (pattern :AST.pattern) ->
      let pattern_string = prettyprint_pattern_color pattern in
      Buffer.add_string buffer pattern_string ;
      Buffer.add_char buffer '\n' ;
      Buffer.add_char buffer '\n')
    compilation_unit.AST.patterns ;
  Buffer.contents buffer
