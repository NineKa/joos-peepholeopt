{
(* NOTE: We aren't going to check the validity of the method and specs during 
         lexing. These checkings are deferred to the weeding phase.           *)
open Parser

let update_pos = fun lexbuf ->
    let curr_pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- { curr_pos with
        Lexing.pos_lnum = curr_pos.Lexing.pos_lnum + 1 ;
        Lexing.pos_bol = curr_pos.Lexing.pos_cnum ;
    }

let collect_token_pos = fun lexbuf ->
    (lexbuf.Lexing.lex_start_p, lexbuf.Lexing.lex_curr_p)

let simulate_eol_before_eof = ref true

let set_simulate_eol_before_eof = fun (flag :bool) ->
  simulate_eol_before_eof := flag

let entered_curly_bracket = ref false

let reset_lexer = fun () ->
  simulate_eol_before_eof := true ;
  entered_curly_bracket := true ;
  ()
                          
}

let newline = ['\n']
let whitespace = [' ' '\t']+

let oct_digit = ['0'-'7']
let hex_digit = ['0'-'9' 'a'-'f' 'A'-'F']
let oct_digit_group_3 = oct_digit oct_digit oct_digit
let hex_digit_group_2 = hex_digit hex_digit
let escape_characters = ['a' 'b' 'f' 'n' 'r' 't' 'v' '\\' '\'' '?' '"']
let escape_sequence = '\\' (escape_characters | oct_digit_group_3 | ('x' hex_digit_group_2))
let java_string_content = ([^'"' '\\' '\r' '\n'] | escape_sequence)*

let java_numeric = ['+' '-']? ('0' | (['1'-'9']['0'-'9']*))

let identifier = ['a'-'z' 'A'-'Z' '_' '$']['a'-'z' 'A'-'Z' '0'-'9' '_' '$']*
let java_name = (identifier '/')* identifier

let java_type_spec_chars = ['a'-'z' 'A'-'Z' '0'-'9' '_' '$' ';' '[']
let java_method_spec = java_name '/' identifier '(' (java_type_spec_chars)* ')' (java_type_spec_chars)*

let comment = ';' [^'\n']*
                   
rule scan = parse
(* keywords *)          
| ".pattern"         {KEYWORD_PATTERN      (collect_token_pos lexbuf)}
| ".schema"          {KEYWORD_SCHEMA       (collect_token_pos lexbuf)}
| ".pre"             {KEYWORD_PRECONDITION (collect_token_pos lexbuf)}
| ".def"             {KEYWORD_DEFINITION   (collect_token_pos lexbuf)}
| ".end"             {KEYWORD_END          (collect_token_pos lexbuf)}
| ".extern"          {KEYWORD_EXTERN       (collect_token_pos lexbuf)}
| "=>"               {KEYWORD_ARROW        (collect_token_pos lexbuf)}
(* operators *)
| "("                {OP_PARAM_LEFT        (collect_token_pos lexbuf)}
| ")"                {OP_PARAM_RIGHT       (collect_token_pos lexbuf)}
| "["                {OP_SQUARE_LEFT       (collect_token_pos lexbuf)}
| "]"                {OP_SQUARE_RIGHT      (collect_token_pos lexbuf)}
| "{"                {entered_curly_bracket := true;  OP_CURLY_LEFT(collect_token_pos lexbuf)}
| "}"                {entered_curly_bracket := false; OP_CURLY_RIGHT(collect_token_pos lexbuf)}
| "<"                {OP_ANGULAR_LEFT      (collect_token_pos lexbuf)}
| ">"                {OP_ANGULAR_RIGHT     (collect_token_pos lexbuf)}
| "="                {OP_ASSIGN            (collect_token_pos lexbuf)}
| "+"                {OP_PLUS              (collect_token_pos lexbuf)}
| "-"                {OP_DASH              (collect_token_pos lexbuf)}
| "*"                {OP_STAR              (collect_token_pos lexbuf)}
| "/"                {OP_SLASH             (collect_token_pos lexbuf)}
| "%"                {OP_REM               (collect_token_pos lexbuf)}
| "||"               {OP_OR                (collect_token_pos lexbuf)}
| "&&"               {OP_AND               (collect_token_pos lexbuf)}
| "=="               {OP_EQ                (collect_token_pos lexbuf)}
| "!="               {OP_NE                (collect_token_pos lexbuf)}
| ">"                {OP_GT                (collect_token_pos lexbuf)}
| "<"                {OP_LT                (collect_token_pos lexbuf)}
| ">="               {OP_GE                (collect_token_pos lexbuf)}
| "<="               {OP_LE                (collect_token_pos lexbuf)}
| ","                {OP_COMMA             (collect_token_pos lexbuf)}
| ":"                {OP_COLON             (collect_token_pos lexbuf)}
(* instructions *)
| "nop"              {INST_NOP             (collect_token_pos lexbuf)}
| "i2c"              {INST_I2C             (collect_token_pos lexbuf)}
| "new"              {INST_NEW             (collect_token_pos lexbuf)}
| "instanceof"       {INST_INSTANCEOF      (collect_token_pos lexbuf)}
| "checkcast"        {INST_CHECKCAST       (collect_token_pos lexbuf)}
| "imul"             {INST_IMUL            (collect_token_pos lexbuf)}
| "ineg"             {INST_INEG            (collect_token_pos lexbuf)}
| "irem"             {INST_IREM            (collect_token_pos lexbuf)}
| "isub"             {INST_ISUB            (collect_token_pos lexbuf)}
| "idiv"             {INST_IDIV            (collect_token_pos lexbuf)}
| "iadd"             {INST_IADD            (collect_token_pos lexbuf)}
| "iinc"             {INST_IINC            (collect_token_pos lexbuf)}
| "goto"             {INST_GOTO            (collect_token_pos lexbuf)}
| "ifeq"             {INST_IFEQ            (collect_token_pos lexbuf)}
| "ifne"             {INST_IFNE            (collect_token_pos lexbuf)}
| "if_acmpeq"        {INST_IF_ACMPEQ       (collect_token_pos lexbuf)}
| "if_acmpne"        {INST_IF_ACMPNE       (collect_token_pos lexbuf)}
| "ifnull"           {INST_IFNULL          (collect_token_pos lexbuf)}
| "ifnonnull"        {INST_IFNONNULL       (collect_token_pos lexbuf)}
| "if_icmpeq"        {INST_IF_ICMPEQ       (collect_token_pos lexbuf)}
| "if_icmpgt"        {INST_IF_ICMPGT       (collect_token_pos lexbuf)}
| "if_icmplt"        {INST_IF_ICMPLT       (collect_token_pos lexbuf)}
| "if_icmple"        {INST_IF_ICMPLE       (collect_token_pos lexbuf)}
| "if_icmpge"        {INST_IF_ICMPGE       (collect_token_pos lexbuf)}
| "if_icmpne"        {INST_IF_ICMPNE       (collect_token_pos lexbuf)}
| "ireturn"          {INST_IRETURN         (collect_token_pos lexbuf)}
| "areturn"          {INST_ARETURN         (collect_token_pos lexbuf)}
| "return"           {INST_RETURN          (collect_token_pos lexbuf)}
| "aload"            {INST_ALOAD           (collect_token_pos lexbuf)}
| "astore"           {INST_ASTORE          (collect_token_pos lexbuf)}
| "iload"            {INST_ILOAD           (collect_token_pos lexbuf)}
| "istore"           {INST_ISTORE          (collect_token_pos lexbuf)}
| "dup"              {INST_DUP             (collect_token_pos lexbuf)}
| "pop"              {INST_POP             (collect_token_pos lexbuf)}
| "swap"             {INST_SWAP            (collect_token_pos lexbuf)}
| "ldc"              {INST_LDC             (collect_token_pos lexbuf)}
| "aconst_null"      {INST_ACONST_NULL     (collect_token_pos lexbuf)}
| "getfield"         {INST_GETFIELD        (collect_token_pos lexbuf)}
| "putfield"         {INST_PUTFIELD        (collect_token_pos lexbuf)}
| "invokevirtual"    {INST_INVOKEVIRTUAL   (collect_token_pos lexbuf)}
| "invokenonvirtual" {INST_INVOKENONVIRTUAL(collect_token_pos lexbuf)}
(* literals *)
| '"' (java_string_content as content) '"' {STRING     (collect_token_pos lexbuf, content)}
| java_numeric as content                  {NUMERIC    (collect_token_pos lexbuf, int_of_string content)}
| java_method_spec as content              {METHOD_SPEC(collect_token_pos lexbuf, content)}
| java_name as content                     {NAME       (collect_token_pos lexbuf, content)}
(* special characters *)
| whitespace {scan lexbuf}
| comment    {scan lexbuf}
| newline
    {
      update_pos lexbuf;
      match !entered_curly_bracket with
      | false -> EOL(collect_token_pos lexbuf)
      | true  -> scan lexbuf
    }
| eof
    {
      match !simulate_eol_before_eof with
      | false -> EOF(collect_token_pos lexbuf)
      | true  -> simulate_eol_before_eof := false ;
                 EOL(collect_token_pos lexbuf)
    }
