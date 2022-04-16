open Parse

let print_error_position lexbuf =
  let open Lexing in 
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_from_file file =
  let lexbuf = file |> open_in |> Lexing.from_channel in 
  match lexbuf |> Parser.toplevel Lexer.read_token with 
  | syntax -> syntax 
  | exception Parser.Error -> 
    let msg = 
      Printf.sprintf "Parser Error at %s" 
        (print_error_position lexbuf) 
    in
    failwith msg 
  | exception Lexer.SyntaxError (_p, e) -> 
    let msg = 
      Printf.sprintf "Syntax Error: %s at %s" 
        e (print_error_position lexbuf) 
    in
    failwith msg 


let () = 
  match parse_from_file "./syntax.delim" with 
  | ts -> print_int (List.length ts)