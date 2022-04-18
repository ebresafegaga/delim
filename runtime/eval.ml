(* a tree walking interpreter *)
open Ast

type value = 
  | VInt of int 
  | VBool of bool 
  | VString of string
  | VList of value list 
  | VClosure of string option * Syntax.name list * env * Syntax.expr 
  | VBuiltin of (value list -> value)

and env = (Syntax.name * value) list 

let rec print_value = function
  | VInt i -> Printf.sprintf "%d" i 
  | VBool b -> Printf.sprintf "%b" b
  | VString s -> Printf.sprintf {|"%s"|} s 
  | VList vs -> 
    vs 
    |> List.map print_value 
    |> String.concat ", "
    |> Printf.sprintf "[%s]"
  | VClosure _ -> "#<closure>"
  | VBuiltin _ -> "#<builtin>" 

let lookup name env = 
  match List.assoc name env with 
  | value -> value 
  | exception Not_found -> failwith (Printf.sprintf "Unbound Variable: %s" name)

let bind name value env = (name, value) :: env 

let rec eval env expr = 
  match expr with
  | Syntax.EVar name -> lookup name env 
  | EInt i -> VInt i 
  | EBool b -> VBool b 
  | EString s -> VString s 
  | EList es -> VList (List.map (eval env) es)
  | ELet (name, expr, body) -> 
      let value = eval env expr in 
      let env = bind name value env in 
      eval env body
  | EIf (p, t, f) -> (
    match eval env p with 
    | VBool true -> eval env t 
    | VBool false -> eval env f 
    | _ -> failwith "Expected a bool expression at if predicate")
  | EFun (params, body) -> VClosure (None, params, env, body)
  | EApply (f, args) -> 
    let args = List.map (eval env) args in 
    match eval env f with
    | VClosure (None, params, closed_env, body) -> 
      let env = List.combine params args @ closed_env in 
      eval env body 
     | VClosure (Some name, params, closed_env, body) as f -> 
      let env = List.combine params args @ closed_env in 
      eval (bind name f env) body 
    | VBuiltin f -> f args
    | _ -> failwith "Expected a function at application"

let rec process_toplevel env top =
  match top with 
  | [] -> []
  | Syntax.TopDefine (name, expr) :: rest -> (
    match eval env expr with 
    | VClosure (None, params, closed_env, body) -> 
      let closure =  VClosure (Some name, params, closed_env, body) in 
      process_toplevel (bind name closure env) rest
    | value -> process_toplevel (bind name value env) rest)
  | TopExpr e :: rest -> eval env e :: process_toplevel env rest  


let ensure_length vs len = 
  let len' = List.length vs in 
  if len' <> len then failwith (Printf.sprintf "Expected %d arguments" len)

let binary ~f args = 
  ensure_length args 2 ; 
  match args with 
  | [VInt a; VInt b] -> VInt (f a b)
  | _ -> failwith "Expected integers as arguments"

let plus = binary ~f:(+)
let minus = binary ~f:(-)
let times = binary ~f:( * )
let div = binary ~f:(/)

let length arg = 
  ensure_length arg 1; 
  match arg with 
  | [VList vs] -> VInt (List.length vs)
  | _ -> failwith "Expected list"

let first arg = 
  ensure_length arg 1; 
  match arg with 
  | [VList vs] -> List.hd vs
  | _ -> failwith "Expected list"

let rest arg = 
  ensure_length arg 1; 
  match arg with 
  | [VList vs] -> VList (List.tl vs)
  | _ -> failwith "Expected list"

let empty arg = 
  ensure_length arg 1; 
  match arg with 
  | [VList vs] -> VBool (List.length vs = 0)
  | _ -> failwith "Expected list"

let primitives = 
    [ "+", plus; 
      "-", minus; 
      "*", times; 
      "/", div; 
      "length", length; 
      "first", first; 
      "rest", rest; 
      "empty", empty ]
    |> List.map (fun (name, value) -> name, VBuiltin value)

let process_toplevel = process_toplevel primitives