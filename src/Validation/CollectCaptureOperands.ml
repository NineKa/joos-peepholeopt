(* 
   For capture patterns, valid operands can have following form:
   - numeric literals
   - string literals
   - names
   - capture names
   - method specifications
   Any other forms of operands will be rejected.
 *)

type capture_operand =
  {pattern_name : string ;
   name         : string ;
   capture_type : Type.operand_type
  }

open ValidationFailed

let pass_id = __FILE__
            
let check_user_defined_instruction_name_valid = fun (manager :exception_manager) (pattern :AST.pattern) ->
  let valid_name_regex = Str.regexp "^[a-zA-Z_$][a-zA-Z0-9_$]*$" in
  let check_single_statement = fun (statement :AST.statement) ->
    match statement with
    | AST.LiteralLabel (_)                 -> ()
    | AST.CaptureLabel (_)                 -> ()
    | AST.BlockOneStmt (_)                 -> ()
    | AST.BlockOneOrMoreStmt (_)           -> ()
    | AST.BlockZeroOrMoreStmt (_)          -> ()
    | AST.Instruction {target = target; _} ->
       match target with
       | AST.InstUserDefined {lexical_info = lexical_info; value = value; _} ->
          if Str.string_match valid_name_regex value 0 then
            ()
          else
            let start_pos = lexical_info.AST.LexicalInfo.start_pos in
            let end_pos = lexical_info.AST.LexicalInfo.end_pos in
            {pass_name = pass_id ;
             reference_sites = [(start_pos, end_pos)] ;
             message = Printf.sprintf "\"%s\" is not a valid instruction alias name." (value) ;
             serverity = Error
            } |> add_new_exception_site manager
       | _                                                                  -> ()
  in List.iter check_single_statement pattern.AST.capture_patterns
   
let check_user_defined_instruction_bounded = fun
    (manager :exception_manager)
    (alias_definitions :CollectAliasDefinitions.user_instruction_alias_definition list)
    (pattern :AST.pattern)
  ->
  let pattern_name = pattern.AST.name in
  let alias_definitions =
    alias_definitions
    |> List.filter (fun (alias_definition) ->
           alias_definition.CollectAliasDefinitions.pattern_name = pattern_name)
  in
  let check_single_statement = fun (statement :AST.statement) ->
    match statement with
    | AST.LiteralLabel (_)        -> ()
    | AST.CaptureLabel (_)        -> ()
    | AST.BlockOneStmt (_)        -> ()
    | AST.BlockOneOrMoreStmt (_)  -> ()
    | AST.BlockZeroOrMoreStmt (_) -> ()
    | AST.Instruction {lexical_info = lexical_info; target = target; operands = operands} ->
       match target with
       | AST.InstUserDefined {value = value} ->
          let open CollectAliasDefinitions in
          if List.exists (fun (alias_definition) -> alias_definition.name = value) alias_definitions then
            ()
          else
            let target_lexical_info = AST.get_instruction_lexical_info target in
            let start_pos = target_lexical_info.AST.LexicalInfo.start_pos in
            let end_pos = target_lexical_info.AST.LexicalInfo.end_pos in
            {pass_name = pass_id ;
             reference_sites = [(start_pos, end_pos)] ;
             message = Printf.sprintf "User defined alias %s is not bounded." value ;
             serverity = Error
            }
            |> add_new_exception_site manager
       | _                                   -> ()
  in
  List.iter check_single_statement (pattern.AST.capture_patterns)

let check_statement_operand_num = fun
    (manager :exception_manager)
    (alias_definitions :CollectAliasDefinitions.user_instruction_alias_definition list)
    (pattern :AST.pattern)
  ->
  let pattern_name = pattern.AST.name in
  let alias_definitions =
    alias_definitions
    |> List.filter (fun (alias_definition) ->
           alias_definition.CollectAliasDefinitions.pattern_name = pattern_name)
  in
  let check_single_statement = fun (statement :AST.statement) ->
    match statement with
    | AST.LiteralLabel (_) -> ()
    | AST.CaptureLabel (_) -> ()
    | AST.BlockOneStmt (_) -> ()
    | AST.BlockOneOrMoreStmt (_) -> ()
    | AST.BlockZeroOrMoreStmt (_) -> ()
    | AST.Instruction {lexical_info = lexical_info; target = target; operands = operands} ->
       let actual_operand_num = List.length operands in
       let expected_operand_num =
         match target with
         | AST.InstUserDefined {value = value} ->
            let open CollectAliasDefinitions in
            List.find (fun (alias_definition) -> alias_definition.name = value) alias_definitions
            |> (fun (alias_definition) -> alias_definition.operand_types)
            |> List.length
         | _ ->
            InstructionInfo.translate_from_ast_node target
            |> InstructionInfo.query_instruction_info
            |> InstructionInfo.select_operands_info
            |> List.length
       in
       if actual_operand_num = expected_operand_num then
         ()
       else
         let start_pos = lexical_info.AST.LexicalInfo.start_pos in
         let end_pos = lexical_info.AST.LexicalInfo.end_pos in
         {pass_name = pass_id ;
          reference_sites = [(start_pos, end_pos)] ;
          message = Printf.sprintf "Capture statement expects %d operands but %d operands are found." expected_operand_num actual_operand_num ;
          serverity = Error
         }
         |> add_new_exception_site manager
  in
  List.iter check_single_statement (pattern.AST.capture_patterns)

let check_statement_operand_form = fun (manager :exception_manager) (pattern :AST.pattern) ->
  let check_single_statement = fun (statement :AST.statement) ->
    let check_single_expression = fun (expression :AST.expression) ->
      let report_site = fun (expression_ast :AST.expression) (expression_name :string) ->
        let lexical_info = AST.get_expression_lexical_info expression_ast in
        let start_pos = lexical_info.AST.LexicalInfo.start_pos in
        let end_pos = lexical_info.AST.LexicalInfo.end_pos in
        {pass_name = pass_id ;
         reference_sites = [(start_pos, end_pos)] ;
         message = Printf.sprintf "Expects capture names or literals, but %s expressions are found." expression_name ;
         serverity = Error
        }
        |> add_new_exception_site manager
      in
      match expression with
      | AST.AddExpr (_)     -> report_site expression "+"
      | AST.SubExpr (_)     -> report_site expression "-"
      | AST.MulExpr (_)     -> report_site expression "*"
      | AST.DivExpr (_)     -> report_site expression "/"
      | AST.RemExpr (_)     -> report_site expression "%"
      | AST.AndExpr (_)     -> report_site expression "&&"
      | AST.OrExpr (_)      -> report_site expression "||"
      | AST.EQExpr (_)      -> report_site expression "=="
      | AST.NEExpr (_)      -> report_site expression "!="
      | AST.GTExpr (_)      -> report_site expression ">"
      | AST.LTExpr (_)      -> report_site expression "<"
      | AST.GEExpr (_)      -> report_site expression ">="
      | AST.LEExpr (_)      -> report_site expression "<="
      | AST.OperandExpr (_) -> ()
      | AST.InvokeExpr (_)  -> report_site expression "invoke"
    in
    match statement with
    | AST.LiteralLabel (_)                     -> ()
    | AST.CaptureLabel (_)                     -> ()
    | AST.BlockOneStmt (_)                     -> ()
    | AST.BlockOneOrMoreStmt (_)               -> ()
    | AST.BlockZeroOrMoreStmt (_)              -> ()
    | AST.Instruction {operands = operands; _} -> List.iter check_single_expression operands
  in
  List.iter check_single_statement pattern.AST.capture_patterns

let check_capture_name_valid = fun (manager :exception_manager) (pattern :AST.pattern) ->
  let check_single_statement = fun (statement :AST.statement) ->
    let check_name = fun (lexical_info :AST.LexicalInfo.t) (name :string) ->
      let valid_name_regex = Str.regexp "^[a-zA-Z_$][a-zA-Z0-9_$]*$" in
      if Str.string_match valid_name_regex name 0 then
        ()
      else
        let start_pos = lexical_info.AST.LexicalInfo.start_pos in
        let end_pos = lexical_info.AST.LexicalInfo.end_pos in
        {pass_name = pass_id ;
         reference_sites = [(start_pos, end_pos)] ;
         message = Printf.sprintf "\"%s\" is not a valid name for capture variable." name ;
         serverity = Error
        } |> (add_new_exception_site manager)
    in
    match statement with
    | AST.LiteralLabel (_) -> ()
    | AST.CaptureLabel {lexical_info = lexical_info; name = name; _} ->
       check_name lexical_info name
    | AST.BlockOneStmt {lexical_info = lexical_info; name = name; _} ->
       check_name lexical_info name
    | AST.BlockOneOrMoreStmt {lexical_info = lexical_info; name = name; _} ->
       check_name lexical_info name
    | AST.BlockZeroOrMoreStmt {lexical_info = lexical_info; name = name; _} ->
       check_name lexical_info name
    | AST.Instruction {operands = operands; _} ->
       let check_single_operand = fun (expression :AST.expression) ->
         match expression with
         | AST.OperandExpr {operand = operand; _} ->
            (match operand with
             | AST.CaptureName {lexical_info = lexical_info; value = value; _} ->
                check_name lexical_info value
             | AST.Name (_)       -> ()
             | AST.String (_)     -> ()
             | AST.Numeric (_)    -> ()
             | AST.MethodSpec (_) -> ())
         | _ -> raise (invalid_arg "perform check_statement_operand_form first.")
       in List.iter check_single_operand operands
  in List.iter check_single_statement pattern.AST.capture_patterns
       
(** @raise ValidationFailed *)
let apply = fun
    (alias_definitions :CollectAliasDefinitions.user_instruction_alias_definition list)
    (unit :AST.compilation_unit)
  ->
  let manager = make_new_exception_manager () in
  let error_checking_barrier = fun () ->
    if has_error manager then
      raise_exception_sites manager
    else
      ()
  in
  List.iter (check_user_defined_instruction_name_valid manager) unit.AST.patterns ;
  error_checking_barrier () ;
  List.iter (check_user_defined_instruction_bounded manager alias_definitions) unit.AST.patterns ;
  error_checking_barrier () ;
  List.iter (check_statement_operand_num manager alias_definitions) unit.AST.patterns ;
  error_checking_barrier () ;
  List.iter (check_statement_operand_form manager) unit.AST.patterns ;
  error_checking_barrier () ;
  List.iter (check_capture_name_valid manager) unit.AST.patterns ;
  error_checking_barrier ()
