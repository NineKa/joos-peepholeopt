type type_spec =
  | Boolean    
  | Byte       
  | Char        
  | Short      
  | Integer    
  | Long       
  | Float      
  | Double     
  | ClassSpec  of (string list)
  | Array      of type_spec  
                
exception ParseTypeSpecAbort of string
exception ParseNameAbort of string

(** @raise ParseNameAbort *)  
let parse_name = fun (name :string) ->
  let parse_stream = Stream.of_string name in
  let rec parse_name' = fun () ->
    let first_char = Stream.peek parse_stream in
    match first_char with
    | None    -> []
    | Some(c) -> let buffer = Buffer.create 16 in
                 let rec scan_until_first_slash = fun () ->
                   let first_char = Stream.peek parse_stream in
                   Stream.junk parse_stream ;
                   match first_char with
                   | None      -> ()
                   | Some('/') -> ()
                   | Some(c')  -> Buffer.add_char buffer c';
                                  scan_until_first_slash ()
                 in
                 scan_until_first_slash () ;
                 (Buffer.contents buffer) :: (parse_name' ())               
  in
  let name_tokens = parse_name' () in
  let name_token_regex = Str.regexp "[a-zA-Z_$][a-zA-Z0-9_$]*" in
  let rec validate_tokens = fun (tokens :string list) ->
    match tokens with
    | []           -> []
    | head :: tail -> if Str.string_match name_token_regex head 0 then
                        head :: (validate_tokens tail)
                      else
                        raise (ParseNameAbort name)
  in
  validate_tokens name_tokens

(** @raise ParseTypeSpecAbort *)
let parse_type_specs = fun (spec_string :string) ->
  let parse_stream = Stream.of_string spec_string in
  let rec scan_single_token = fun () ->
    let first_char = Stream.peek parse_stream in
    Stream.junk parse_stream ;
    match first_char with
    | None    -> None
    | Some(c) ->
       match c with
       | 'Z' -> Some(Boolean)
       | 'B' -> Some(Byte)
       | 'C' -> Some(Char)
       | 'S' -> Some(Short)
       | 'I' -> Some(Integer)
       | 'J' -> Some(Long)
       | 'F' -> Some(Float)
       | 'D' -> Some(Double)
       | 'L' -> let buffer = Buffer.create 32 in
                let rec collect_until_semicolon = fun () ->
                  let first_char' = Stream.peek parse_stream in
                  Stream.junk parse_stream ;
                   match first_char' with
                   | None      -> raise (ParseTypeSpecAbort spec_string)
                   | Some(';') -> (
                     try Some(ClassSpec(parse_name (Buffer.contents buffer)))
                     with ParseNameAbort(what) ->
                       raise (ParseTypeSpecAbort spec_string))
                   | Some(c')  -> Buffer.add_char buffer c' ;
                                  collect_until_semicolon ()
                in
                collect_until_semicolon ()
       | '[' -> (match scan_single_token () with
                 | None    -> raise (ParseTypeSpecAbort spec_string)
                 | Some(t) -> Some(Array(t)))
       | _   -> raise (ParseTypeSpecAbort spec_string)
  in
  let rec scan_until_eof = fun () ->
    match scan_single_token () with
    | None -> []
    | Some(t) -> t :: (scan_until_eof ())
  in
  scan_until_eof ()

(** @raise ParseTypeSpecAbort *)
let parse_type_spec = fun (spec_string :string) ->
  match parse_type_specs spec_string with
  | head :: tail -> head
  | []           -> raise (ParseTypeSpecAbort spec_string)


exception CreatASTNodeAbort of Lexing.position * Lexing.position * string

module LexicalInfo = struct
  type t = 
    { start_pos : Lexing.position ;
      end_pos   : Lexing.position 
    }
    
  let make_lexical_info = fun (start_pos' :Lexing.position) (end_pos' :Lexing.position) ->
    {start_pos = start_pos';
     end_pos = end_pos'
    }

  let raise_error_here = fun (lexical_info :t) (message :string) ->
    CreatASTNodeAbort (lexical_info.start_pos, lexical_info.end_pos, message)
end
                                                          
type method_spec = string list * string * type_spec list * type_spec 
                 
type operand =
  | CaptureName of { lexical_info : LexicalInfo.t; value : string      }
  | Name        of { lexical_info : LexicalInfo.t; value : string list }
  | String      of { lexical_info : LexicalInfo.t; value : string      }
  | Numeric     of { lexical_info : LexicalInfo.t; value : int         }
  | MethodSpec  of { lexical_info : LexicalInfo.t; value : method_spec }

let get_operand_lexical_info = fun (operand :operand) ->
  match operand with
  | CaptureName({lexical_info = lexical_info'; _}) -> lexical_info'
  | Name({lexical_info = lexical_info'; _})        -> lexical_info'
  | String({lexical_info = lexical_info'; _})      -> lexical_info'
  | Numeric({lexical_info = lexical_info'; _})     -> lexical_info'
  | MethodSpec({lexical_info = lexical_info'; _})  -> lexical_info'
                 
exception ParseMethodSpecAbort of string

(** @raise ParseMethodSpecAbort *)
let parse_method_spec = fun (spec_string :string) ->
  let split_delimiter = Str.regexp "[()]" in
  match Str.split split_delimiter spec_string with
  | [name; param_type; ret_type] -> 
     (try
        let parsed_names = parse_name name in
        let parsed_param_type = parse_type_specs param_type in
        let parsed_ret_type = parse_type_spec ret_type in
        let rec collect_and_return =
          fun (tokens :string list) (namespace_token :string list) ->
          match tokens with
          | head :: []   ->
             (List.rev namespace_token, head,
              parsed_param_type, parsed_ret_type)
          | head :: tail -> collect_and_return tail (head :: namespace_token)
          | []           -> raise (ParseMethodSpecAbort spec_string)
        in
        collect_and_return parsed_names []
      with ParseNameAbort(_)     -> raise (ParseMethodSpecAbort spec_string)
         | ParseTypeSpecAbort(_) -> raise (ParseTypeSpecAbort spec_string))
  | _ -> raise (ParseMethodSpecAbort spec_string)

(** @raise CreatASTNodeAbort *)
let make_capture_name = fun (start_pos, end_pos) (content :string) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try
    match parse_name content with
    | [ capture_name ] ->
       CaptureName
         {lexical_info = lexical_info ;
          value        = capture_name
         }
    | _                -> raise (LexicalInfo.raise_error_here lexical_info content)
  with ParseNameAbort(_) ->
    raise (LexicalInfo.raise_error_here lexical_info content)

(** @raise CreatASTNodeAbort *)
let make_name = fun (start_pos, end_pos) (content :string) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try Name
        {lexical_info = lexical_info ;
         value        = parse_name content ;
        }
  with ParseNameAbort(_) ->
    raise (LexicalInfo.raise_error_here lexical_info content)

let make_string = fun (start_pos, end_pos) (content :string) ->
  String
    {lexical_info     = LexicalInfo.make_lexical_info start_pos end_pos ;
     value            = content
    }

(** @raise CreatASTNodeAbort *)
let make_method_spec = fun (start_pos, end_pos) (content :string) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try MethodSpec
        { lexical_info = lexical_info ;
          value        = parse_method_spec content
        }
  with ParseMethodSpecAbort(_) ->
    raise (LexicalInfo.raise_error_here lexical_info content)

let make_numeric = fun (start_pos, end_pos) (content :int) ->
  Numeric
    {lexical_info      = LexicalInfo.make_lexical_info start_pos end_pos ;
     value             = content
    }


type expression =
  (* Arithematics *)
  | AddExpr of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | SubExpr of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | MulExpr of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | DivExpr of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | RemExpr of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  (* Logical *)
  | AndExpr of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | OrExpr  of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | EQExpr  of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | NEExpr  of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | GTExpr  of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | LTExpr  of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | GEExpr  of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  | LEExpr  of {lexical_info : LexicalInfo.t; lhs : expression; rhs : expression}
  (* Primitive *)
  | OperandExpr of {lexical_info : LexicalInfo.t; operand : operand }
  | InvokeExpr  of {lexical_info : LexicalInfo.t; target : string; operands : expression list}

let get_expression_lhs = fun (expression :expression) ->
  match expression with
  | AddExpr {lhs = ret_expression; _} -> ret_expression
  | SubExpr {lhs = ret_expression; _} -> ret_expression
  | MulExpr {lhs = ret_expression; _} -> ret_expression
  | DivExpr {lhs = ret_expression; _} -> ret_expression
  | RemExpr {lhs = ret_expression; _} -> ret_expression
  | AndExpr {lhs = ret_expression; _} -> ret_expression
  | OrExpr  {lhs = ret_expression; _} -> ret_expression
  | EQExpr  {lhs = ret_expression; _} -> ret_expression
  | NEExpr  {lhs = ret_expression; _} -> ret_expression
  | GTExpr  {lhs = ret_expression; _} -> ret_expression
  | LTExpr  {lhs = ret_expression; _} -> ret_expression
  | GEExpr  {lhs = ret_expression; _} -> ret_expression
  | LEExpr  {lhs = ret_expression; _} -> ret_expression
  | OperandExpr(_)  -> invalid_arg "argument is not a valid binary expression"
  | InvokeExpr(_)   -> invalid_arg "argument is not a valid binary expression"

let get_expression_rhs = fun (expression :expression) ->
  match expression with
  | AddExpr {rhs = ret_expression; _} -> ret_expression
  | SubExpr {rhs = ret_expression; _} -> ret_expression
  | MulExpr {rhs = ret_expression; _} -> ret_expression
  | DivExpr {rhs = ret_expression; _} -> ret_expression
  | RemExpr {rhs = ret_expression; _} -> ret_expression
  | AndExpr {rhs = ret_expression; _} -> ret_expression
  | OrExpr  {rhs = ret_expression; _} -> ret_expression
  | EQExpr  {rhs = ret_expression; _} -> ret_expression
  | NEExpr  {rhs = ret_expression; _} -> ret_expression
  | GTExpr  {rhs = ret_expression; _} -> ret_expression
  | LTExpr  {rhs = ret_expression; _} -> ret_expression
  | GEExpr  {rhs = ret_expression; _} -> ret_expression
  | LEExpr  {rhs = ret_expression; _} -> ret_expression
  | OperandExpr(_) -> invalid_arg "argument is not a valid binary expression"
  | InvokeExpr(_)  -> invalid_arg "argument is not a valid binary expression"

let rec is_expression_extended = fun (expression :expression) ->
  match expression with
  | OperandExpr(_) -> false
  | InvokeExpr(_)  -> true
  | _              -> let lhs = get_expression_lhs expression in
                      let rhs = get_expression_rhs expression in
                      (is_expression_extended lhs) || (is_expression_extended rhs)
                 
let get_expression_lexical_info = fun (expression :expression) ->
  match expression with
  | AddExpr({lexical_info = lexical_info'; _})     -> lexical_info'
  | SubExpr({lexical_info = lexical_info'; _})     -> lexical_info'
  | MulExpr({lexical_info = lexical_info'; _})     -> lexical_info'
  | DivExpr({lexical_info = lexical_info'; _})     -> lexical_info'
  | RemExpr({lexical_info = lexical_info'; _})     -> lexical_info'
  | AndExpr({lexical_info = lexical_info'; _})     -> lexical_info'
  | OrExpr ({lexical_info = lexical_info'; _})     -> lexical_info'
  | EQExpr ({lexical_info = lexical_info'; _})     -> lexical_info'
  | NEExpr ({lexical_info = lexical_info'; _})     -> lexical_info'
  | GTExpr ({lexical_info = lexical_info'; _})     -> lexical_info'
  | LTExpr ({lexical_info = lexical_info'; _})     -> lexical_info'
  | GEExpr ({lexical_info = lexical_info'; _})     -> lexical_info'
  | LEExpr ({lexical_info = lexical_info'; _})     -> lexical_info'
  | OperandExpr({lexical_info = lexical_info'; _}) -> lexical_info'
  | InvokeExpr({lexical_info = lexical_info'; _})  -> lexical_info'                                                                                    
                                                    
let make_expression_operand = fun (operand :operand) ->
  let lexical_info = get_operand_lexical_info operand in
  let start_pos = lexical_info.LexicalInfo.start_pos in
  let end_pos = lexical_info.LexicalInfo.end_pos in
  OperandExpr
    {lexical_info = LexicalInfo.make_lexical_info start_pos end_pos;
     operand = operand
    }

let make_expression_binary = fun (lhs_expr :expression) (rhs_expr :expression) constructor ->
  let lhs_lexical_info = get_expression_lexical_info lhs_expr in
  let rhs_lexical_info = get_expression_lexical_info rhs_expr in
  let start_pos = lhs_lexical_info.LexicalInfo.start_pos in
  let end_pos = rhs_lexical_info.LexicalInfo.end_pos in
  constructor (LexicalInfo.make_lexical_info start_pos end_pos) lhs_expr rhs_expr 

let make_expression_add = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    AddExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_sub = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    SubExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_mul = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    MulExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_div = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    DivExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor
    
let make_expression_rem = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    RemExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_or = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    OrExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_and = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    AndExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_eq = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    EQExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_ne = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    NEExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_gt = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    GTExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_lt = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    LTExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_ge = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    GEExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

let make_expression_le = fun (lhs_expr :expression) (rhs_expr :expression) ->
  let constructor = fun lexical_info lhs_expr' rhs_expr' ->
    LEExpr {lexical_info = lexical_info ; lhs = lhs_expr' ; rhs = rhs_expr'}
  in
  make_expression_binary lhs_expr rhs_expr constructor

(** @raise CreatASTNodeAbort *)
let make_expression_invoke = fun (start_pos, end_pos) target_name (operands :expression list) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try
    match parse_name target_name with
    | [ target ] -> InvokeExpr
                      {lexical_info = lexical_info ;
                       target = target ;
                       operands = operands
                      }
    | _          -> raise (LexicalInfo.raise_error_here lexical_info target_name)
  with ParseNameAbort(_) ->
    raise (LexicalInfo.raise_error_here lexical_info target_name)
      
type instruction =
  | InstNop              of {lexical_info : LexicalInfo.t}
  | InstI2c              of {lexical_info : LexicalInfo.t}
  | InstNew              of {lexical_info : LexicalInfo.t}
  | InstInstanceof       of {lexical_info : LexicalInfo.t}
  | InstCheckcast        of {lexical_info : LexicalInfo.t}
  | InstImul             of {lexical_info : LexicalInfo.t}
  | InstIneg             of {lexical_info : LexicalInfo.t}
  | InstIrem             of {lexical_info : LexicalInfo.t}
  | InstIsub             of {lexical_info : LexicalInfo.t}
  | InstIdiv             of {lexical_info : LexicalInfo.t}
  | InstIadd             of {lexical_info : LexicalInfo.t}
  | InstIinc             of {lexical_info : LexicalInfo.t}
  | InstGoto             of {lexical_info : LexicalInfo.t}
  | InstIfeq             of {lexical_info : LexicalInfo.t}
  | InstIfne             of {lexical_info : LexicalInfo.t}
  | InstIf_acmpeq        of {lexical_info : LexicalInfo.t}
  | InstIf_acmpne        of {lexical_info : LexicalInfo.t}
  | InstIfnull           of {lexical_info : LexicalInfo.t}
  | InstIfnonnull        of {lexical_info : LexicalInfo.t}
  | InstIf_icmpeq        of {lexical_info : LexicalInfo.t}
  | InstIf_icmpgt        of {lexical_info : LexicalInfo.t}
  | InstIf_icmplt        of {lexical_info : LexicalInfo.t}
  | InstIf_icmple        of {lexical_info : LexicalInfo.t}
  | InstIf_icmpge        of {lexical_info : LexicalInfo.t}
  | InstIf_icmpne        of {lexical_info : LexicalInfo.t}
  | InstIreturn          of {lexical_info : LexicalInfo.t}
  | InstAreturn          of {lexical_info : LexicalInfo.t}
  | InstReturn           of {lexical_info : LexicalInfo.t}
  | InstAload            of {lexical_info : LexicalInfo.t}
  | InstAstore           of {lexical_info : LexicalInfo.t}
  | InstIload            of {lexical_info : LexicalInfo.t}
  | InstIstore           of {lexical_info : LexicalInfo.t}
  | InstDup              of {lexical_info : LexicalInfo.t}
  | InstPop              of {lexical_info : LexicalInfo.t}
  | InstSwap             of {lexical_info : LexicalInfo.t}
  | InstLdc              of {lexical_info : LexicalInfo.t}
  | InstAconst_null      of {lexical_info : LexicalInfo.t}
  | InstGetfield         of {lexical_info : LexicalInfo.t}
  | InstPutfield         of {lexical_info : LexicalInfo.t}
  | InstInvokevirtual    of {lexical_info : LexicalInfo.t}
  | InstInvokenonvirtual of {lexical_info : LexicalInfo.t}
  | InstUserDefined      of {lexical_info : LexicalInfo.t ; value : string}

let get_instruction_lexical_info = fun (instruction :instruction) ->
  match instruction with
  | InstNop{lexical_info = lexical_info ; _}              -> lexical_info
  | InstI2c{lexical_info = lexical_info ; _}              -> lexical_info
  | InstNew{lexical_info = lexical_info ; _}              -> lexical_info
  | InstInstanceof{lexical_info = lexical_info ; _}       -> lexical_info
  | InstCheckcast{lexical_info = lexical_info ; _}        -> lexical_info
  | InstImul{lexical_info = lexical_info ; _}             -> lexical_info
  | InstIneg{lexical_info = lexical_info ; _}             -> lexical_info
  | InstIrem{lexical_info = lexical_info ; _}             -> lexical_info
  | InstIsub{lexical_info = lexical_info ; _}             -> lexical_info
  | InstIdiv{lexical_info = lexical_info ; _}             -> lexical_info
  | InstIadd{lexical_info = lexical_info ; _}             -> lexical_info
  | InstIinc{lexical_info = lexical_info ; _}             -> lexical_info
  | InstGoto{lexical_info = lexical_info ; _}             -> lexical_info
  | InstIfeq{lexical_info = lexical_info ; _}             -> lexical_info
  | InstIfne{lexical_info = lexical_info ; _}             -> lexical_info
  | InstIf_acmpeq{lexical_info = lexical_info ; _}        -> lexical_info
  | InstIf_acmpne{lexical_info = lexical_info ; _}        -> lexical_info
  | InstIfnull{lexical_info = lexical_info ; _}           -> lexical_info
  | InstIfnonnull{lexical_info = lexical_info ; _}        -> lexical_info
  | InstIf_icmpeq{lexical_info = lexical_info ; _}        -> lexical_info
  | InstIf_icmpgt{lexical_info = lexical_info ; _}        -> lexical_info
  | InstIf_icmplt{lexical_info = lexical_info ; _}        -> lexical_info
  | InstIf_icmple{lexical_info = lexical_info ; _}        -> lexical_info
  | InstIf_icmpge{lexical_info = lexical_info ; _}        -> lexical_info
  | InstIf_icmpne{lexical_info = lexical_info ; _}        -> lexical_info
  | InstIreturn{lexical_info = lexical_info ; _}          -> lexical_info
  | InstAreturn{lexical_info = lexical_info ; _}          -> lexical_info
  | InstReturn{lexical_info = lexical_info ; _}           -> lexical_info
  | InstAload{lexical_info = lexical_info ; _}            -> lexical_info
  | InstAstore{lexical_info = lexical_info ; _}           -> lexical_info
  | InstIload{lexical_info = lexical_info ; _}            -> lexical_info
  | InstIstore{lexical_info = lexical_info ; _}           -> lexical_info
  | InstDup{lexical_info = lexical_info ; _}              -> lexical_info
  | InstPop{lexical_info = lexical_info ; _}              -> lexical_info
  | InstSwap{lexical_info = lexical_info ; _}             -> lexical_info
  | InstLdc{lexical_info = lexical_info ; _}              -> lexical_info
  | InstAconst_null{lexical_info = lexical_info ; _}      -> lexical_info
  | InstGetfield{lexical_info = lexical_info ; _}         -> lexical_info
  | InstPutfield{lexical_info = lexical_info ; _}         -> lexical_info
  | InstInvokevirtual{lexical_info = lexical_info ; _}    -> lexical_info
  | InstInvokenonvirtual{lexical_info = lexical_info ; _} -> lexical_info
  | InstUserDefined{lexical_info = lexical_info ; _}      -> lexical_info
                          
let make_instruction_nop = fun (start_pos, end_pos) ->
  InstNop {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_i2c = fun (start_pos, end_pos) ->
  InstI2c {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_new = fun (start_pos, end_pos) ->
  InstNew {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_instanceof = fun (start_pos, end_pos) ->
  InstInstanceof {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}
  
let make_instruction_checkcast = fun (start_pos, end_pos) ->
  InstCheckcast {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_imul = fun (start_pos, end_pos) ->
  InstImul {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_ineg = fun (start_pos, end_pos) ->
  InstIneg {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_irem = fun (start_pos, end_pos) ->
  InstIrem {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_isub = fun (start_pos, end_pos) ->
  InstIsub {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_idiv = fun (start_pos, end_pos) ->
  InstIdiv {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_iadd = fun (start_pos, end_pos) ->
  InstIadd {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_iinc = fun (start_pos, end_pos) ->
  InstIinc {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_goto = fun (start_pos, end_pos) ->
  InstGoto {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_ifeq = fun (start_pos, end_pos) ->
  InstIfeq {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_ifne = fun (start_pos, end_pos) ->
  InstIfne {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_if_acmpeq = fun (start_pos, end_pos) ->
  InstIf_acmpeq {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_if_acmpne = fun (start_pos, end_pos) ->
  InstIf_acmpne {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_ifnull = fun (start_pos, end_pos) ->
  InstIfnull {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_ifnonnull = fun (start_pos, end_pos) ->
  InstIfnonnull {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_if_icmpeq = fun (start_pos, end_pos) ->
  InstIf_icmpeq {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_if_icmpgt = fun (start_pos, end_pos) ->
  InstIf_icmpgt {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_if_icmplt = fun (start_pos, end_pos) ->
  InstIf_icmplt {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_if_icmple = fun (start_pos, end_pos) ->
  InstIf_icmple {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_if_icmpge = fun (start_pos, end_pos) ->
  InstIf_icmpge {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}
  
let make_instruction_if_icmpne = fun (start_pos, end_pos) ->
  InstIf_icmpne {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_ireturn = fun (start_pos, end_pos) ->
  InstIreturn {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_areturn = fun (start_pos, end_pos) ->
  InstAreturn {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_return = fun (start_pos, end_pos) ->
  InstReturn {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_aload = fun (start_pos, end_pos) ->
  InstAload {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_astore = fun (start_pos, end_pos) ->
  InstAstore {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_iload = fun (start_pos, end_pos) ->
  InstIload {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_istore = fun (start_pos, end_pos) ->
  InstIstore {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_dup = fun (start_pos, end_pos) ->
  InstDup {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_pop = fun (start_pos, end_pos) ->
  InstPop {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_swap = fun (start_pos, end_pos) ->
  InstSwap {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_ldc = fun (start_pos, end_pos) ->
  InstLdc {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_aconst_null = fun (start_pos, end_pos) ->
  InstAconst_null {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_getfield = fun (start_pos, end_pos) ->
  InstGetfield {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_putfield = fun (start_pos, end_pos) ->
  InstPutfield {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_invokevirtual = fun (start_pos, end_pos) ->
  InstInvokevirtual {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}

let make_instruction_invokenonvirtual = fun (start_pos, end_pos) ->
  InstInvokenonvirtual {lexical_info = (LexicalInfo.make_lexical_info start_pos end_pos)}
  
type statement =
  | LiteralLabel        of {lexical_info : LexicalInfo.t ; name : string}
  | CaptureLabel        of {lexical_info : LexicalInfo.t ; name : string}
  | BlockOneStmt        of {lexical_info : LexicalInfo.t ; name : string}
  | BlockOneOrMoreStmt  of {lexical_info : LexicalInfo.t ; name : string}
  | BlockZeroOrMoreStmt of {lexical_info : LexicalInfo.t ; name : string}
  | Instruction         of {lexical_info : LexicalInfo.t ; target : instruction ; operands :expression list}

let get_statement_lexcial_info = fun (statement :statement) ->
  match statement with
  | LiteralLabel {lexical_info = lexical_info; _}        -> lexical_info
  | CaptureLabel {lexical_info = lexical_info; _}        -> lexical_info
  | BlockOneStmt {lexical_info = lexical_info; _}        -> lexical_info
  | BlockOneOrMoreStmt {lexical_info = lexical_info; _}  -> lexical_info
  | BlockZeroOrMoreStmt {lexical_info = lexcial_info; _} -> lexcial_info
  | Instruction {lexical_info = lexical_info; _}         -> lexical_info
                         
(** @raise CreatASTNodeAbort *)
let make_statement_label = fun end_pos (operand :operand) ->
  let operand_lexical_info = get_operand_lexical_info operand in
  let start_pos = operand_lexical_info.LexicalInfo.start_pos in
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  match operand with
  | Name {value = names}       ->
     (match names with
      | [ name ] -> LiteralLabel {lexical_info = lexical_info ; name = name}
      | _        -> raise (LexicalInfo.raise_error_here lexical_info ""))
  | CaptureName {value = name} ->
     CaptureLabel {lexical_info = lexical_info ; name = name}
  | _ -> raise (LexicalInfo.raise_error_here lexical_info "")
     
let make_statement_block_one_stmt = fun (start_pos, end_pos) (content :string) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try
    let names = parse_name content in
    match names with
    | [ name ] -> BlockOneStmt {lexical_info = lexical_info ; name = name}
    | _        -> raise (LexicalInfo.raise_error_here lexical_info content)
  with ParseNameAbort(what) ->
    raise (LexicalInfo.raise_error_here lexical_info what)

let make_statement_block_one_or_more_stmt = fun (start_pos, end_pos) (content :string) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try
    let names = parse_name content in
    match names with
    | [ name ] -> BlockOneOrMoreStmt {lexical_info = lexical_info ; name = name}
    | _        -> raise (LexicalInfo.raise_error_here lexical_info content)
  with ParseNameAbort(what) ->
    raise (LexicalInfo.raise_error_here lexical_info what)

let make_statement_block_zero_or_more_stmt = fun (start_pos, end_pos) (content :string) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try
    let names = parse_name content in
    match names with
    | [ name ] -> BlockZeroOrMoreStmt {lexical_info = lexical_info ; name = name}
    | _        -> raise (LexicalInfo.raise_error_here lexical_info content)
  with ParseNameAbort(what) ->
    raise (LexicalInfo.raise_error_here lexical_info what)

let make_statement_instruction_single = fun (instruction :instruction) ->
  let lexical_info = get_instruction_lexical_info instruction in
  Instruction
    {lexical_info = lexical_info ;
     target = instruction ;
     operands = []
    }

let make_statement_instruction_with_operands = fun (start_pos, end_pos) (insturction :instruction) (operands :expression list) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  Instruction
    {lexical_info = lexical_info ;
     target = insturction ;
     operands = operands
    }

let make_statement_instruction_with_operands_infer_pos = fun (instruction :instruction) (operands :expression list) ->
  let rec get_last_expression = fun (expressions :expression list) ->
    match expressions with
    | []             -> raise (Failure "implementation error in parser")
    | [ expression ] -> expression
    | head :: tail   -> get_last_expression tail
  in
  let instruction_lexical_info = get_instruction_lexical_info instruction in
  let expression_lexical_info = get_expression_lexical_info (get_last_expression operands) in
  let start_pos = instruction_lexical_info.LexicalInfo.start_pos in
  let end_pos = expression_lexical_info.LexicalInfo.end_pos in
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  Instruction
    {lexical_info = lexical_info ;
     target = instruction ;
     operands = operands
    }

(** @raise CreatASTNodeAbort *)
let make_statement_instruction_from_operand = fun (operand :operand )->
  let operand_lexical_info = get_operand_lexical_info operand in
  let start_pos = operand_lexical_info.LexicalInfo.start_pos in
  let end_pos = operand_lexical_info.LexicalInfo.end_pos in
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  match operand with
  | Name {value = names; _} ->
     (match names with
      | [ name ] ->
         Instruction
           {lexical_info = lexical_info ;
            target = InstUserDefined {lexical_info = lexical_info; value = name} ;
            operands = [] 
           }
      | _ -> raise (LexicalInfo.raise_error_here lexical_info ""))
  | _ -> raise (LexicalInfo.raise_error_here lexical_info "")

(** @raise CreatASTNodeAbort *)
let make_statement_instruction_from_expressions = fun (expressions :expression list) ->
  let rec get_last_expression = fun (expressions :expression list) ->
    match expressions with
    | []             -> raise (Failure "implementation error in parser")
    | [ expression ] -> expression
    | head :: tail   -> get_last_expression tail
  in
  let rec get_first_expression = fun (expressions :expression list) ->
    match expressions with
    | []                 -> raise (Failure "implementation error in parser")
    | expression :: tail -> (expression, tail)
  in
  let (first_expression, args) = get_first_expression expressions in
  let last_expression = get_last_expression expressions in
  let first_lexical_info = get_expression_lexical_info first_expression in
  let last_lexical_info = get_expression_lexical_info last_expression in
  let start_pos = first_lexical_info.LexicalInfo.start_pos in
  let end_pos = last_lexical_info.LexicalInfo.end_pos in
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  match first_expression with
  | OperandExpr {operand = operand; _} ->
     (match operand with
      | Name {value = names; _} ->
         (match names with
          | [ name ] ->
             Instruction
               {lexical_info = lexical_info ;
                target = InstUserDefined {lexical_info = lexical_info; value = name} ;
                operands = args
               }
          | _ -> raise (LexicalInfo.raise_error_here lexical_info ""))
      | _ -> raise (LexicalInfo.raise_error_here lexical_info ""))
  | _ -> raise (LexicalInfo.raise_error_here lexical_info "")

       
type definition =
  {lexical_info : LexicalInfo.t ;
   name         : string ;
   alternatives : instruction list
  }

let get_definition_lexical_info = fun (definition :definition) ->
  let {lexical_info = lexical_info; _} = definition in
  lexical_info

(** @raise CreatASTNodeAbort *)
let make_definition = fun (start_pos, end_pos) (name :string) (altrs :instruction list) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try
    match parse_name name with
    | [ name ] -> {lexical_info = lexical_info ;
                   name = name ;
                   alternatives = altrs
                  }
    | _ -> raise (LexicalInfo.raise_error_here lexical_info name)
  with ParseNameAbort(what) ->
    raise (LexicalInfo.raise_error_here lexical_info what)
      
type pattern =
  {lexical_info     : LexicalInfo.t ;
   name             : string ;
   preconditions    : expression list ;
   definitions      : definition list ;
   capture_patterns : statement list ;
   rewrite_patterns : statement list
  }

(** @raise CreatASTNodeAbort *)
let make_pattern_new = fun (start_pos, end_pos) (name :string) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try
    match parse_name name with
    | [ name ] -> {lexical_info = lexical_info ;
                   name = name ;
                   preconditions = [] ;
                   definitions = [] ;
                   capture_patterns = [] ;
                   rewrite_patterns = []
                  }
    | _ -> raise (LexicalInfo.raise_error_here lexical_info name)
  with ParseNameAbort(_) ->
    raise (LexicalInfo.raise_error_here lexical_info name)

let make_pattern_add_precondition = fun (pattern :pattern) (precondition :expression) ->
  let {preconditions = preconditions; _} = pattern in
  {pattern with
    preconditions = preconditions @ [ precondition ]
  }

let make_pattern_add_definition = fun (pattern :pattern) (definition :definition) ->
  let {definitions = definitions; _} = pattern in
  {pattern with
    definitions = definitions @ [definition]
  }

let make_pattern_add_capture_pattern = fun (pattern :pattern) (stmt :statement) ->
  let {capture_patterns = capture_patterns; _} = pattern in
  {pattern with
    capture_patterns = capture_patterns @ [ stmt ]
  }

let make_pattern_add_rewrite_pattern = fun (pattern :pattern) (stmt :statement) ->
  let {rewrite_patterns = rewrite_patterns; _} = pattern in
  {pattern with
    rewrite_patterns = rewrite_patterns @ [ stmt ]
  }

let make_pattern_finalize = fun (pattern :pattern) end_pos ->
  let {lexical_info = lexical_info; _} = pattern in
  let start_pos = lexical_info.LexicalInfo.start_pos in
  {pattern with
    lexical_info = LexicalInfo.make_lexical_info start_pos end_pos
  }

  
type schema_element =
  | InternalPattern of {lexical_info : LexicalInfo.t; name : string}
  | ExternalPattern of {lexical_info : LexicalInfo.t; name : string}

let get_schema_element_lexical_info = fun (schema_element :schema_element) ->
  match schema_element with
  | InternalPattern {lexical_info = lexical_info; _} -> lexical_info
  | ExternalPattern {lexical_info = lexical_info; _} -> lexical_info

(** @raise CreatASTNodeAbort *)
let make_schema_element_internal = fun (start_pos, end_pos) pattern_name ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try
    match parse_name pattern_name with
    | [ pattern_name ] -> InternalPattern
                            {lexical_info = lexical_info ;
                             name = pattern_name
                            }
    | _ -> raise (LexicalInfo.raise_error_here lexical_info pattern_name)
  with ParseNameAbort(what) ->
    raise (LexicalInfo.raise_error_here lexical_info what)

(** @raise CreatASTNodeAbort *)
let make_schema_element_external = fun (start_pos, end_pos) pattern_name ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  try
    match parse_name pattern_name with
    | [ pattern_name ] -> ExternalPattern
                            {lexical_info = lexical_info ;
                             name = pattern_name
                            }
    | _ -> raise (LexicalInfo.raise_error_here lexical_info pattern_name)
  with ParseNameAbort(what) ->
    raise (LexicalInfo.raise_error_here lexical_info what)

          
type schema =
  {lexical_info : LexicalInfo.t ;
   schema : schema_element list
  }

let get_schema_lexical_info = fun (schema :schema) ->
  let {lexical_info = lexical_info; _} = schema in
  lexical_info
  
let make_schema = fun (start_pos, end_pos) (schema :schema_element list) ->
  let lexical_info = LexicalInfo.make_lexical_info start_pos end_pos in
  {lexical_info = lexical_info ;
   schema = schema
  }


type compilation_unit =
  {schemas  :schema list ;
   patterns :pattern list
  }

let make_compilation_unit_new = fun () -> {schemas = [] ; patterns = []}

let make_compilation_unit_add_schema = fun compilation_unit schema ->
  let {schemas = schemas; _} = compilation_unit in
  {compilation_unit with
    schemas = schema :: schemas ;
  }

let make_compilation_unit_add_pattern = fun compilation_unit pattern ->
  let {patterns = patterns; _} = compilation_unit in
  {compilation_unit with
    patterns = pattern :: patterns
  }
