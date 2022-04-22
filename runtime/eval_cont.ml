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

let combine ~params ~args = 
  let pl = List.length params and al = List.length args in
  if pl <> al then 
    failwith (Printf.sprintf "Expected %d arguments, but got %d" pl al)
  else List.combine params args  

type cont = 
  | ContEnd
  | ContList of env * value list * Syntax.expr list * cont  
  | ContLet of env * Syntax.name * Syntax.expr * cont 
  | ContIf of env * Syntax.expr * Syntax.expr * cont 
  | ContF of env * Syntax.expr list * cont 
  | ContArg of env * value * value list * Syntax.expr list * cont 

let rec eval env expr cont =
  match expr with
  | Syntax.EVar name -> apply_cont cont (lookup name env)
  | EInt i -> apply_cont cont (VInt i) 
  | EBool b ->  apply_cont cont (VBool b)
  | EString s ->  apply_cont cont (VString s)
  | EList [] -> apply_cont cont (VList [])
  | EList (e :: es) -> eval env e (ContList (env, [], es, cont)) 
  | ELet (name, expr, body) -> eval env expr (ContLet (env, name, body, cont)) 
  | EIf (p, t, f) -> 
    eval env p (ContIf (env, t, f, cont))
  | EFun (params, body) -> apply_cont cont @@ VClosure (None, params, env, body)
  | EApply (f, args) -> eval env f (ContF (env, args, cont))

and apply_cont : cont -> value -> value = fun cont value -> 
    match cont with 
    | ContEnd -> value
    | ContList (_, vs, [], cont) -> 
      let vs = List.rev (value :: vs) in 
      apply_cont cont (VList vs)
    | ContList (env, vs, e :: es, cont) -> 
      eval env e (ContList (env, value :: vs, es, cont))
    | ContLet (env, name, body, cont) -> 
      eval (bind name value env) body cont 
    | ContIf (env, t, f, cont) -> (
      match value with 
      | VBool true -> eval env t cont 
      | VBool false -> eval env f cont 
      | _ -> failwith "Expected a bool expression at if predicate")
    | ContF (_env, [], cont) -> apply_procedure ~lambda:value ~args:[] cont
    | ContF (env, arg :: args, cont) -> 
      eval env arg (ContArg (env, value, [], args, cont)) 
    | ContArg (_env, f, args, [], cont) -> 
      let args = List.rev (value :: args) in 
      apply_procedure ~lambda:f ~args cont 
    | ContArg (env, f, argsv, arge :: argse, cont) -> 
      eval env arge (ContArg (env, f, value :: argsv, argse, cont))

and apply_procedure ~lambda ~args cont =
  match lambda with
  | VClosure (None, params, closed_env, body) -> 
    let env = combine ~params ~args @ closed_env in 
    eval env body cont (* TCO *)
  | VClosure (Some name, params, closed_env, body) as f -> 
    let env = combine ~params ~args @ closed_env in 
    eval (bind name f env) body cont (* TCO *)
  | VBuiltin f -> apply_cont cont (f args)
  | _ -> failwith "Expected a function at application"
    
type bounce = V of value | B of (unit -> bounce)

type d = 
  { lambda: value; 
    args: value list; 
    cont: cont }

let rec trampoline : bounce -> value = function 
  | V value -> value 
  | B b -> trampoline (b ())

let eval env e = eval env e ContEnd


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

let cons args = 
  ensure_length args 2; 
  match args with 
  | [any; VList vs] -> VList (any :: vs)
  | _ -> failwith "Expected a list"

let print arg = 
  ensure_length arg 1; 
  print_endline (print_value (List.hd arg)) ;
  VBool false

let lt args = 
  ensure_length args 2;
  match args with 
  | [VInt a; VInt b] -> VBool (a < b)
  | _ -> failwith "Expected integers as arguments"

let gt args = 
  ensure_length args 2;
  match args with 
  | [VInt a; VInt b] -> VBool (a > b)
  | _ -> failwith "Expected integers as arguments"

let primitives = 
    [ "+", plus; 
      "-", minus; 
      "*", times; 
      "/", div; 
      "<", lt; 
      ">", gt;
      "length", length; 
      "first", first; 
      "rest", rest;
      "cons", cons; 
      "empty", empty; 
      "print", print]
    |> List.map (fun (name, value) -> name, VBuiltin value)

let process_toplevel = process_toplevel primitives