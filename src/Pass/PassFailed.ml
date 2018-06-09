type serverity =
  | Error
  | Warning

type exception_site =
  {pass_name       :string ;
   reference_sites :(Lexing.position * Lexing.position) list ;
   message         :string ;
   serverity       :serverity
  }

exception PassFailed of exception_site list

type exception_site_manager = exception_site list ref

let make_new_exception_manager = fun () ->
  let new_exception_manager = ref [] in
  new_exception_manager

let add_new_exception_site = fun (manager :exception_site_manager) (site :exception_site) ->
  manager := (!manager) @ [ site ] ;
  ()

let raise_exception_manager = fun (manager :exception_site_manager) ->
  raise (PassFailed (!manager))

let has_exception_site = fun (manager :exception_site_manager) ->
  List.length (!manager) <> 0

let has_warning_exception_site = fun (manager :exception_site_manager) ->
  List.exists
    (fun {serverity = serverity; _} -> serverity = Warning)
    (!manager)

let has_error_exception_site = fun (manager :exception_site_manager) ->
  List.exists
    (fun {serverity = serverity; _} -> serverity = Error)
    (!manager)

let make_new_exception_site = fun
  (pass_name       :string)
  (reference_sites :(Lexing.position * Lexing.position) list)
  (message         :string)
  (serverity       :serverity) ->
  {pass_name       = pass_name ;
   reference_sites = reference_sites ;
   message         = message ;
   serverity       = serverity
  }

let get_exception_site_list = fun (manager :exception_site_manager) ->
  !manager

let exception_site_manager_error_barrier = fun (manager :exception_site_manager) ->
  match has_error_exception_site manager with
  | true  -> raise_exception_manager manager
  | false -> ()

let exception_site_manager_warning_barrier = fun (manager :exception_site_manager) ->
  match has_warning_exception_site manager with
  | true  -> raise_exception_manager manager
  | false -> ()

let exception_site_manager_barrier = fun (manager : exception_site_manager) ->
  match has_exception_site manager with
  | true  -> raise_exception_manager manager
  | false -> ()
