(*
  For user instruction alias definition we check the following preconditions:
  - instruction alias cannot be redefined within the same pattern
  - instruction alias must have operand types agreed 
 *)

type user_instruction_alias_definition =
  {pattern_name  : string ;
   name          : string ;
   alternatives  : InstructionInfo.builtin_instruction list ;
   operand_types : InstructionInfo.operand_type list
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

let collect_alias_definition = fun (manager :exception_manager) (pattern :AST.pattern) ->
  let pattern_name = pattern.AST.name in
  let collected_alias_definition_option = pattern.AST.definitions |>
  List.map (fun (defintion :AST.definition) ->
      let rec build_validating_pairs = fun alternatives ->
        match alternatives with
        | head :: tail -> let rec collect = fun list ->
                            match list with
                            | head' :: tail' -> (head, head') :: (collect tail')
                            | []             -> []
                          in (collect tail) @ (build_validating_pairs tail)
        | []           -> []
      in
      let contains_unmatched_operand =
        build_validating_pairs defintion.AST.alternatives
        |> List.fold_left (fun (rest :site option) ((alias_a, alias_b) :(AST.instruction * AST.instruction)) -> 
               match rest with
               | Some site -> Some site
               | None ->
                  let inst_a = InstructionInfo.translate_from_ast_node alias_a |> InstructionInfo.query_instruction_info in
                  let inst_b = InstructionInfo.translate_from_ast_node alias_b |> InstructionInfo.query_instruction_info in
                  let operand_a = InstructionInfo.select_operands_info inst_a in
                  let operand_b = InstructionInfo.select_operands_info inst_b in
                  if operand_a = operand_b then
                    None
                  else
                    let inst_a_lexical_info = AST.get_instruction_lexical_info alias_a in
                    let start_pos_a = inst_a_lexical_info.AST.LexicalInfo.start_pos in
                    let end_pos_a = inst_a_lexical_info.AST.LexicalInfo.end_pos in
                    let inst_b_lexical_info = AST.get_instruction_lexical_info alias_b in
                    let start_pos_b = inst_b_lexical_info.AST.LexicalInfo.start_pos in
                    let end_pos_b = inst_b_lexical_info.AST.LexicalInfo.end_pos in
                    Some
                      {pass_name = pass_id ;
                       reference_sites = [(start_pos_a, end_pos_a); (start_pos_b, end_pos_b)] ;
                       message = Printf.sprintf "Instructions operands are not agreed." ;
                       serverity = Error
                      }
             ) None
      in
      match contains_unmatched_operand with
      | Some site -> add_new_exception_site manager site; None
      | None      -> let alternativs = List.map InstructionInfo.translate_from_ast_node defintion.AST.alternatives in
                     Some
                       {pattern_name = pattern_name ;
                        name = defintion.AST.name ;
                        alternatives = alternativs ;
                        operand_types = List.hd alternativs |> InstructionInfo.query_instruction_info |> InstructionInfo.select_operands_info
                       }
    )
  in
  let rec remove_none = fun list ->
    match list with
    | Some head :: tail -> head :: (remove_none tail)
    | None      :: tail -> remove_none tail
    | []                -> []
  in remove_none collected_alias_definition_option
  
(** @raise ValidationFailed *)
let apply = fun (unit :AST.compilation_unit) ->
  let manager = make_new_exception_manager () in
  List.iter (check_redefination manager) unit.AST.patterns ;
  if has_error manager then
    raise_exception_sites manager
  else
    let alias_definitions = 
      unit.AST.patterns
      |> List.map (collect_alias_definition manager)
      |> List.fold_left (@) []
    in
    if has_error manager then
      raise_exception_sites manager
    else
      alias_definitions
