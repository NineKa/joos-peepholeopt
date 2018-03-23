(* 
   For capture patterns, valid operands can have following form:
   - numeric literals
   - string literals
   - names
   - capture names
   - method specifications
   Any other forms of operands will be rejected.
 *)

open ValidationFailed

let pass_id = __FILE__

let apply_expression = fun (manager :exception_manager) (expression :AST.expression) ->
  let build_site = fun (lexical_info :AST.LexicalInfo.t) ->
    let start_pos = lexical_info.AST.LexicalInfo.start_pos in
    let end_pos = lexical_info.AST.LexicalInfo.end_pos in
    {pass_name = pass_id ;
     reference_sites = [(start_pos, end_pos)] ;
     message = "<empty>" ;
     serverity = ValidationFailed.Error
    }
  in
  let format_message = fun (expression_name :string) ->
    Printf.sprintf "Expected capture variables while %s were found." expression_name
  in
  match expression with
  (* Arithematics *)
  | AST.AddExpr {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "+ expressions")}
  | AST.SubExpr {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "- expressions")}
  | AST.MulExpr {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "* expressions")}
  | AST.DivExpr {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "/ expressions")}
  | AST.RemExpr {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "% expressions")}
  (* Logical *)
  | AST.AndExpr {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "&& expressions")}
  | AST.OrExpr  {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "|| expressions")}
  | AST.EQExpr  {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "== expressions")}
  | AST.NEExpr  {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "!= expressions")}
  | AST.GTExpr  {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "> expressions")}
  | AST.LTExpr  {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "< expressions")}
  | AST.GEExpr  {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message ">= expressions")}
  | AST.LEExpr  {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "<= expressions")}
  (* Primitive *)
  | AST.OperandExpr (_) -> ()
  | AST.InvokeExpr {lexical_info = lexical_info; _} ->
     add_new_exception_site manager {(build_site lexical_info) with
         message = (format_message "invoke expressions")}

let apply_statement = fun (manager :exception_manager) (statement :AST.statement) ->
  match statement with
  | AST.LiteralLabel (_)                     -> ()
  | AST.CaptureLabel (_)                     -> ()
  | AST.BlockOneStmt (_)                     -> ()
  | AST.BlockOneOrMoreStmt (_)               -> ()
  | AST.BlockZeroOrMoreStmt (_)              -> ()
  | AST.Instruction {operands = operands; _} -> List.iter (apply_expression manager) operands

let apply_pattern = fun (manager :exception_manager) (pattern :AST.pattern) ->
  let capture_patterns = pattern.AST.capture_patterns in
  List.iter (apply_statement manager) capture_patterns
                                              
(** @raise ValidationFailed *)
let apply = fun (unit :AST.compilation_unit) ->
  let manager = make_new_exception_manager () in
  let patterns = unit.AST.patterns in
  List.iter (apply_pattern manager) patterns ;
  if has_error manager then
    raise_exception_sites manager
  else
    to_list manager
