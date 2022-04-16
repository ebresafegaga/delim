open Parse
open Ast

let () = 
  let file = "./examples/syntax.delim" in 
  let result = Parser.from_file file in 
  match result with 
  | Ok syntax -> 
    syntax
    |> List.iter (fun top -> Printf.printf "%s\n" (Syntax.print_toplevel top))
  | Error e -> print_endline e 
