let pass_name = __FILE__

let collect_distinct_pattern_names = fun (unit :AST.compilation_unit) ->
  let rec collect_distinct_pattern_names' = fun
    (patterns       :AST.pattern list)
    (distinct_names :string list) ->
    match patterns with
    | pattern :: rest ->
      let {AST.name = name; _} = pattern in
      if List.exists (fun name' -> name' = name) distinct_names then
        collect_distinct_pattern_names' rest distinct_names
      else
        collect_distinct_pattern_names' rest (name :: distinct_names)
    | _               -> distinct_names
  in
  collect_distinct_pattern_names' unit.AST.patterns []

let check_pattern_name_uniqueness = fun
  (unit :AST.compilation_unit)
  (name :string) ->
  let patterns = unit.AST.patterns in
  let patterns_with_given_name =
    List.filter (fun {AST.name = name'; _} -> name' = name) patterns
  in
  if List.length patterns_with_given_name = 1 then
    None
  else
    let reference_sites =
      List.map
        (fun ({AST.lexical_info = lexical_info; _} :AST.pattern) ->
           let start_pos = lexical_info.AST.LexicalInfo.start_pos in
           let end_pos = lexical_info.AST.LexicalInfo.end_pos in
           (start_pos, end_pos))
        patterns_with_given_name
    in
    let message = Printf.sprintf "Found duplicate pattern \"%s\"." name in
    Some
      {PassFailed.pass_name = pass_name ;
       PassFailed.reference_sites = reference_sites ;
       PassFailed.message = message ;
       PassFailed.serverity = PassFailed.Error
      }

let apply = fun (unit :AST.compilation_unit) ->
  let manager = PassFailed.make_new_exception_manager () in
  collect_distinct_pattern_names unit
  |> List.iter
    (fun name ->
       match check_pattern_name_uniqueness unit name with
       | None                -> ()
       | Some exception_site ->
         PassFailed.add_new_exception_site manager exception_site) ;
  PassFailed.exception_site_manager_barrier manager ;
  ()
