let pass_name = __FILE__

let collect_distinct_alias_names = fun (pattern :AST.pattern) ->
  let rec collect_distinct_alias_names' = fun 
    (alias_definitions :AST.definition list) 
    (alias_names       :string list) ->
    match alias_definitions with
    | [] -> alias_names
    | alias_definition :: rest ->
      let defined_alias_name = alias_definition.AST.name in
      if List.exists (fun name -> name = defined_alias_name) alias_names then
        collect_distinct_alias_names' rest alias_names
      else
        collect_distinct_alias_names' rest (defined_alias_name :: alias_names)
  in
  collect_distinct_alias_names' pattern.AST.definitions []

let check_alias_name_uniqueness = fun 
  (pattern            :AST.pattern) 
  (defined_alias_name :string) ->
  let definition_with_given_name = 
    pattern.AST.definitions
    |> List.filter 
      (fun (definition :AST.definition) -> 
         definition.AST.name = defined_alias_name)
  in 
  if List.length definition_with_given_name = 1 then
    None
  else
    let reference_sites = 
      definition_with_given_name
      |> List.map
        (fun ({AST.lexical_info = lexical_info; _} :AST.definition) ->
           let start_pos = lexical_info.AST.LexicalInfo.start_pos in
           let end_pos = lexical_info.AST.LexicalInfo.end_pos in
           (start_pos, end_pos)
        )
    in
    let message = Printf.sprintf 
        "Found duplciate alias definition name \"%s\"." defined_alias_name in 
    Some
      {PassFailed.pass_name = pass_name;
       PassFailed.reference_sites = reference_sites;
       PassFailed.message = message;
       PassFailed.serverity = Error
      }

let apply_pattern = fun (pattern :AST.pattern) -> 
  let manager = PassFailed.make_new_exception_manager () in 
  collect_distinct_alias_names pattern 
  |> List.iter
    (fun (alias_name :string) -> 
       match check_alias_name_uniqueness pattern alias_name with 
       | None -> ()
       | Some exception_site ->
         PassFailed.add_new_exception_site manager exception_site) ;
  manager

let apply = fun (unit :AST.compilation_unit) ->
  unit.AST.patterns
  |> List.iter (fun (pattern :AST.pattern) ->
      let manager = apply_pattern pattern in
      PassFailed.exception_site_manager_barrier manager ;
      ())