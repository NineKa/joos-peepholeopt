let rec lex_print_until_eof = fun (lex_func :Lexing.lexbuf -> Parser.token) (lexbuf :Lexing.lexbuf) ->
  let token = lex_func lexbuf in
  Printf.printf "%s\n" (PrettyPrint.prettyprint_token token) ;
  match token with
  | Parser.EOF(_) -> ()
  | _ -> lex_print_until_eof lex_func lexbuf

let type_string = "Ljava/io/InputStream;"
let method_string = "java/io/InputStream/read([BII)I"
let operand_string = "10 + foo(hoo, [A]) * 3"

let print_use_stream = fun (str :string) ->
  let print_stream = Stream.of_string str in 
  let rec print_use_stream' = fun () ->
    let first_char = Stream.peek print_stream in
    Stream.junk print_stream ;
    match first_char with
    | None -> ()
    | Some(c) -> let _ = Printf.printf "%c\n" c in print_use_stream' ()
  in
  print_use_stream' ()


let rec read_into_buffer_until_eof = fun (b :Buffer.t) ->
  let bytes = Bytes.create 32 in
  match input stdin bytes 0 32 with
  | 0 -> ()
  | _ -> Buffer.add_bytes b bytes ; read_into_buffer_until_eof b

let print_token_scan = fun (scan :Lexing.lexbuf -> Parser.token) lexbuf ->
  let token = scan lexbuf in
  Printf.printf "%s\n" (PrettyPrint.prettyprint_token token) ;
  token
  
let main =
  (*
  Lexer.set_simulate_eol_before_eof false ;
  let scanner = print_token_scan Lexer.scan in
  let ast = Parser.extended_expression_debug scanner (Lexing.from_string operand_string) in
  let print_content = PrettyPrint.prettyprint_expression ast in 
  Printf.printf "%s\n" print_content 
   *)
  
  
  Printf.printf "Start!\n" ;
  Lexer.set_simulate_eol_before_eof true ;
  let lexbuf = print_token_scan Lexer.scan in
  let ast = Parser.compilation_unit lexbuf (Lexing.from_channel stdin) in
  Printf.printf "%s" (PrettyPrint.prettyprint_compilation_unit ast)
  
   
