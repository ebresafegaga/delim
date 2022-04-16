
let print_error_position lexbuf =
  let open Lexing in 
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_from_lexbuf lexbuf = 
  match Grammar.toplevel Lexer.read_token lexbuf with 
  | syntax -> Ok syntax 
  | exception Grammar.Error -> 
    let msg = 
      Printf.sprintf "Parser Error at %s" 
        (print_error_position lexbuf) 
    in
    Error msg 
  | exception Lexer.SyntaxError (_p, e) -> 
    let msg = 
      Printf.sprintf "Syntax Error: %s at %s" 
        e (print_error_position lexbuf) 
    in
    Error msg 

let parse_from_string s = s |> Lexing.from_string |> parse_from_lexbuf
let parse_from_file name = 
  name 
  |> open_in 
  |> Lexing.from_channel
  |> parse_from_lexbuf