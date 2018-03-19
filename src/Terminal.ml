module InputBufferMap = Map.Make(struct type t = int let compare = compare end)
                      
type terminal =
  {mutable buffer_map         : (Buffer.t) InputBufferMap.t ;
   mutable name_map           : string InputBufferMap.t ;
   mutable buffer_num_counter : int ;
  }

let new_terminal = fun () ->
  {buffer_map = InputBufferMap.empty ;
   name_map = InputBufferMap.empty ;
   buffer_num_counter = 0
  }
  
let load_input_channel = fun (terminal :terminal) (in_channel :in_channel) (name :string) ->
  let allocated_number = terminal.buffer_num_counter in
  terminal.buffer_num_counter <- terminal.buffer_num_counter + 1 ;
  let new_buffer_map =
    InputBufferMap.add allocated_number (Buffer.create 32) terminal.buffer_map
  in
  let new_name_map =
    InputBufferMap.add allocated_number name terminal.name_map
  in
  terminal.buffer_map <- new_buffer_map ;
  terminal.name_map <- new_name_map ;
  let refill_function = fun (bytes :bytes) (max_size :int) ->
    let buffer_map = terminal.buffer_map in
    let buffer = InputBufferMap.find allocated_number buffer_map in
    let actual_size = input in_channel bytes 0 max_size in
    Buffer.add_bytes buffer (Bytes.sub bytes 0 actual_size) ;
    actual_size
  in
  let lexbuf = Lexing.from_function refill_function in
  lexbuf.Lexing.lex_curr_p <-
    {lexbuf.Lexing.lex_curr_p with
      pos_fname = string_of_int allocated_number
    } ;
  lexbuf
  
let load_new_file = fun (terminal :terminal) (path :string) ->
  let file_in_channel = open_in path in
  load_input_channel terminal file_in_channel path

let get_input_buffer = fun (terminal :terminal) (lexbuf :Lexing.lexbuf) ->
  let allocated_number = int_of_string (lexbuf.Lexing.lex_curr_p.pos_fname) in
  let buffer_map = terminal.buffer_map in
  try
    InputBufferMap.find allocated_number buffer_map
  with Not_found -> raise (invalid_arg "Lexingbuf seems not managed by such terminal.")

let get_input_buffer_from_position = fun (terminal :terminal) (lexing_pos :Lexing.position) ->
  let allocated_number = int_of_string lexing_pos.pos_fname in
  let buffer_map = terminal.buffer_map in
  try
    InputBufferMap.find allocated_number buffer_map
  with Not_found -> raise (invalid_arg "Lexingbuf seems not managed by such terminal.")
    
let get_input_name = fun (terminal :terminal) (lexbuf :Lexing.lexbuf) ->
  let allocated_number = int_of_string (lexbuf.Lexing.lex_curr_p.pos_fname) in
  let name_map = terminal.name_map in
  try
    InputBufferMap.find allocated_number name_map
  with Not_found -> raise (invalid_arg "Lexingbuf seems not managed by such terminal.")
  
let get_input_name_from_position = fun (terminal :terminal) (lexing_pos :Lexing.position) ->
  let allocated_number = int_of_string lexing_pos.pos_fname in
  let name_map = terminal.name_map in
  try
    InputBufferMap.find allocated_number name_map
  with Not_found -> raise (invalid_arg "Lexingbuf seems not managed by such terminal.")

let tailor_string = fun (buffer :Buffer.t) (start_pos :Lexing.position) (end_pos :Lexing.position) ->
  let rec find_next_bol = fun (content :string) (pos :int) ->
    if not (pos < String.length content) then
      String.length content
    else
      if String.get content pos = '\n' then
        pos
      else
        find_next_bol content (pos + 1)
  in
  let content = Buffer.contents buffer in
  let start_char_pos = start_pos.Lexing.pos_cnum in
  let end_char_pos = end_pos.Lexing.pos_cnum in
  let start_print_pos = start_pos.Lexing.pos_bol in
  let end_print_pos = find_next_bol content end_char_pos in
  let pre_underline = String.sub content start_print_pos (start_char_pos - start_print_pos) in
  let in_underline = String.sub content start_char_pos (end_char_pos - start_char_pos) in
  let post_underline = String.sub content end_char_pos (end_print_pos - end_char_pos) in
  let buffer = Buffer.create 32 in
  Buffer.add_string buffer pre_underline ;
  Buffer.add_string buffer (ANSITerminal.sprintf [ANSITerminal.Underlined] "%s" in_underline) ;
  Buffer.add_string buffer post_underline ;
  Buffer.contents buffer
    
let raise_error = fun (terminal :terminal) (positions :(Lexing.position * Lexing.position) list) (info :string) ->
  let buffer = Buffer.create 32 in
  Buffer.add_string buffer (ANSITerminal.sprintf [ANSITerminal.red ; ANSITerminal.Bold] "%s" "Error") ;
  Buffer.add_string buffer (Printf.sprintf ": %s\n" info) ;
  let rec print_code_context = fun (positions :(Lexing.position * Lexing.position) list) ->
    match positions with
    | (start_pos, end_pos) :: tail ->
       assert (start_pos.pos_fname = end_pos.pos_fname) ;
       let file_name = get_input_name_from_position terminal start_pos in
       let line_number = start_pos.Lexing.pos_lnum in
       let char_number = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1 in
       Buffer.add_string buffer (Printf.sprintf "File \"%s\", line %d, start from character %d:\n" file_name line_number char_number) ;
       Buffer.add_string buffer (tailor_string (get_input_buffer_from_position terminal start_pos) start_pos end_pos) ;
       Buffer.add_char buffer '\n' ;
       print_code_context tail
    | []                                   -> ()
  in
  print_code_context positions ;
  Printf.eprintf "%s" (Buffer.contents buffer) 

let raise_warning = fun (terminal :terminal) (positions :(Lexing.position * Lexing.position) list) (info :string) ->
  let buffer = Buffer.create 32 in
  Buffer.add_string buffer (ANSITerminal.sprintf [ANSITerminal.yellow ; ANSITerminal.Bold] "%s" "Warning") ;
  Buffer.add_string buffer (Printf.sprintf ": %s\n" info) ;
  let rec print_code_context = fun (positions :(Lexing.position * Lexing.position) list) ->
    match positions with
    | (start_pos, end_pos) :: tail ->
       assert (start_pos.pos_fname = end_pos.pos_fname) ;
       let file_name = get_input_name_from_position terminal start_pos in
       let line_number = start_pos.Lexing.pos_lnum in
       let char_number = start_pos.Lexing.pos_cnum - start_pos.Lexing.pos_bol + 1 in
       Buffer.add_string buffer (Printf.sprintf "File \"%s\", line %d, start from character %d:\n" file_name line_number char_number) ;
       Buffer.add_string buffer (tailor_string (get_input_buffer_from_position terminal start_pos) start_pos end_pos) ;
       Buffer.add_char buffer '\n' ;
       print_code_context tail
    | []                                   -> ()
  in
  print_code_context positions ;
  Printf.eprintf "%s" (Buffer.contents buffer)

let raise_warning_single = fun (terminal :terminal) (start_pos :Lexing.position) (end_pos :Lexing.position) (info :string) ->
  raise_warning terminal [(start_pos, end_pos)] info

let raise_error_single = fun (terminal :terminal) (start_pos :Lexing.position) (end_pos :Lexing.position) (info :string) ->
  raise_error terminal [(start_pos, end_pos)] info
