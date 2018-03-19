type serverity =
  | Error
  | Warning

type site =
  {pass_name :string ;
   reference_sites :(Lexing.position * Lexing.position) list ;
   message :string ;
   serverity :serverity
  }
  
exception ValidationFailed of site list

type exception_manager = site list ref
                            
let make_new_exception_manager = fun () ->
  let new_exception_manager = ref [] in
  new_exception_manager

let add_new_exception_site = fun (manager :exception_manager) (except :site) ->
  manager := (!manager) @ [ except ] ;
  ()

let add_new_exception_site_and_raise = fun (manager :exception_manager) (except :site) ->
  manager := (!manager) @ [ except ] ;
  raise (ValidationFailed (!manager))

let raise_exception_sites = fun (manager :exception_manager) ->
  raise (ValidationFailed (!manager))

let is_empty = fun (manager :exception_manager) ->
  List.length (!manager) == 0

let has_warning = fun (manager :exception_manager) ->
  List.exists (fun {serverity = serverity; _} -> serverity = Warning) (!manager)

let has_error = fun (manager :exception_manager) ->
  List.exists (fun {serverity = serverity; _} -> serverity = Error) (!manager)

let to_list = fun (manager :exception_manager) ->
  (!manager)
  
let make_site = fun (pass_name :string) (reference_sites :(Lexing.position * Lexing.position) list) (message :string) (serverity :serverity) ->
  {pass_name = pass_name ;
   reference_sites = reference_sites ;
   message = message ;
   serverity = serverity
  }
