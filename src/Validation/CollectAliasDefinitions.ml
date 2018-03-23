(*
  For user instruction alias definition we check the following preconditions:
  - instruction alias cannot be redefined within the same pattern
  - instruction alias must have number and types of operands agreed 
 *)

type user_instruction_alias_definition =
  {pattern_name  : string ;
   name          : string ;
   alternatives  : InstructionInfo.builtin_instruction list ;
   operand_types : Type.operand_type list
  }

open ValidationFailed
  
let pass_id = __FILE__

let check_redefination = fun (manager :exception_manager) (pattern :AST.pattern) ->
  let rec select_distinct_elements = fun list ->
    let rec contains = fun list x ->
      match list with
      | head :: tail -> if x = head then true else contains tail x
      | []           -> false
    in
    match list with
    | head :: tail -> if contains tail head then
                        select_distinct_elements tail
                      else
                        head :: select_distinct_elements tail
    | []           -> []
  in
  let defined_names =
    pattern.AST.definitions
    |> (List.map (fun (definition :AST.definition) -> definition.AST.name))
    |> select_distinct_elements
  in
  let check_if_duplicate = fun (name :string) ->
    let patterns_with_such_name =
      pattern.AST.definitions
      |> List.filter (fun (definition :AST.definition) -> definition.AST.name = name)
    in
    if (List.length patterns_with_such_name) > 1 then
      (* redefinition found *)
      let collected_reference_sites =
        patterns_with_such_name
        |> List.map (fun (definition :AST.definition) ->
               let lexical_info = definition.AST.lexical_info in
               let start_pos = lexical_info.AST.LexicalInfo.start_pos in
               let end_pos = lexical_info.AST.LexicalInfo.end_pos in
               (start_pos, end_pos))
      in
      {pass_name = pass_id ;
       reference_sites = collected_reference_sites ;
       message = Printf.sprintf "Instruction alias %s seems to be redefined." name ;
       serverity = Error
      } |> (add_new_exception_site manager)
    else
      ()
  in List.iter check_if_duplicate defined_names

let check_alias_definition_name_valid = fun (manager :exception_manager) (pattern :AST.pattern) ->
  let check_single_definition = fun (alias_definition :AST.definition) ->
    let valid_name_regex = Str.regexp "^[a-zA-Z_$][a-zA-Z0-9_$]*$" in
    if Str.string_match valid_name_regex (alias_definition.AST.name) 0 then
      ()
    else
      let lexical_info = alias_definition.AST.lexical_info in
      let start_pos = lexical_info.AST.LexicalInfo.start_pos in
      let end_pos = lexical_info.AST.LexicalInfo.end_pos in
      {pass_name = pass_id ;
       reference_sites = [(start_pos, end_pos)] ;
       message = Printf.sprintf "\"%s\" is not a valid instruction alias name." (alias_definition.AST.name) ;
       serverity = Error
      } |> (add_new_exception_site manager)
  in List.iter check_single_definition pattern.AST.definitions
   
let collect_alias_definition = fun (manager :exception_manager) (pattern :AST.pattern) ->
  (* if operands is agreed, None will be return. Otherwise, a exception site will be returned *)
  let check_definition_agreed = fun (definition :AST.definition) ->
    let rec build_element_pairs = fun list ->
      match list with
      | head :: tail -> let rec collect_tail = fun list' ->
                          match list' with
                          | head' :: tail' -> (head, head') :: (collect_tail tail')
                          | []             -> []
                        in (collect_tail tail) @ (build_element_pairs tail)
      | []           -> []
    in
    let validate_element_pair = fun ((ast_a, ast_b) :(AST.instruction * AST.instruction)) ->
      let operands_info_a = InstructionInfo.translate_from_ast_node ast_a
                            |> InstructionInfo.query_instruction_info
                            |> InstructionInfo.select_operands_info
      and operands_info_b = InstructionInfo.translate_from_ast_node ast_b
                            |> InstructionInfo.query_instruction_info
                            |> InstructionInfo.select_operands_info
      in
      if operands_info_a = operands_info_b then
        None
      else
        let lexical_info_a = AST.get_instruction_lexical_info ast_a in
        let lexical_info_b = AST.get_instruction_lexical_info ast_b in
        let start_pos_a = lexical_info_a.AST.LexicalInfo.start_pos in
        let end_pos_a = lexical_info_a.AST.LexicalInfo.end_pos in
        let start_pos_b = lexical_info_b.AST.LexicalInfo.start_pos in
        let end_pos_b = lexical_info_b.AST.LexicalInfo.end_pos in
        Some
          {pass_name = pass_id ;
           reference_sites = [(start_pos_a, end_pos_a); (start_pos_b, end_pos_b)] ;
           message = Printf.sprintf "In user defined alias %s, instructions operands are not agreed." (definition.AST.name) ;
           serverity = Error
          }
    in
    definition.AST.alternatives
    |> build_element_pairs
    |> List.fold_left
         (fun rest pair -> match rest with
                           | Some error_site -> Some error_site
                           | None            -> validate_element_pair pair)
         None
  in
  let rec collect_alias = fun (definitions :AST.definition list) ->
    match definitions with
    | definition :: tail ->
       (match check_definition_agreed definition with
        | Some site -> add_new_exception_site manager site ;
                       collect_alias tail
        | None      ->
           let alternatives = List.map InstructionInfo.translate_from_ast_node definition.AST.alternatives in
           {pattern_name = pattern.AST.name ;
            name = definition.AST.name ;
            alternatives = alternatives ;
            operand_types = List.hd alternatives
                            |> InstructionInfo.query_instruction_info
                            |> InstructionInfo.select_operands_info
           } :: (collect_alias tail))
    | []                 -> []
  in
  collect_alias pattern.AST.definitions
   
(** @raise ValidationFailed *)
let apply = fun (unit :AST.compilation_unit) ->
  let manager = make_new_exception_manager () in
  let error_checking_barrier = fun () ->
    if has_error manager then
      raise_exception_sites manager
    else
      ()
  in
  List.iter (check_alias_definition_name_valid manager) unit.AST.patterns ;
  error_checking_barrier () ;
  List.iter (check_redefination manager) unit.AST.patterns ;
  error_checking_barrier () ;
  let alias_definitions = 
    unit.AST.patterns
    |> List.map (collect_alias_definition manager)
    |> List.fold_left (@) []
  in
  error_checking_barrier () ;
  alias_definitions

(* to_string *)
let user_instruction_alias_definition_to_json =
  fun (alias :user_instruction_alias_definition) ->
  let open Yojson in
  let alternatives =
    List.map
      (fun alternative ->
        `String (InstructionInfo.builtin_instruction_to_string alternative))
      alias.alternatives
  in
  let operand_types =
    List.map
      (fun operand_type ->
        `String (InstructionInfo.operand_type_to_string operand_type))
      alias.operand_types
  in
  `Assoc
    [("pattern_name" , `String (alias.pattern_name)) ;
     ("name"         , `String (alias.name)) ;
     ("alternatives" , `List (alternatives)) ;
     ("operand_types", `List (operand_types))
    ]
  
let user_instruction_alias_definitions_to_json =
  fun (alias_set :user_instruction_alias_definition list) ->
  `List (List.map user_instruction_alias_definition_to_json alias_set)

let user_instruction_alias_definition_to_string =
  fun (alias :user_instruction_alias_definition) ->
  user_instruction_alias_definition_to_json alias
  |> Yojson.to_string

let user_instruction_alias_definitions_to_string =
  fun (alias_set :user_instruction_alias_definition list) ->
  user_instruction_alias_definitions_to_json alias_set
  |> Yojson.to_string
