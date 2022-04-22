open Parse
open Runtime

let () = 
  let file = Sys.argv.(1) in 
  let result = Parser.from_file file in 
  match result with 
  | Ok syntax -> 
    syntax
    |> Eval_cont.process_toplevel
    |> List.iter (fun top -> Printf.printf "%s\n" (Eval_cont.print_value top))
  | Error e -> print_endline e 

