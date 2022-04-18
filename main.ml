open Parse
open Runtime

let () = 
  let file = "./examples/a.delim" in 
  let result = Parser.from_file file in 
  match result with 
  | Ok syntax -> 
    syntax
    |> Eval.process_toplevel
    |> List.iter (fun top -> Printf.printf "%s\n" (Eval.print_value top))
  | Error e -> print_endline e 
