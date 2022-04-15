
let parse_from_file file =
  file
  |> open_in 
  |> Lexing.from_channel
  |> Parser.toplevel Lexer.read_token

let () = 
  let ts = parse_from_file "./syntax.delim" in 
  print_int (List.length ts) 