type name = string

type expr =
  | EVar of name
  | EString of string
  | EInt of int
  | EBool of bool
  | EList of expr list
  | ELet of name * expr * expr
  | EFun of name list * expr
  | EIf of expr * expr * expr
  | EApply of expr * expr list

type toplevel = TopDefine of name * expr | TopExpr of expr 

let rec print_expr e = 
  match e with 
  | EVar name -> Printf.sprintf "VAR[%s]" name
  | EString s -> Printf.sprintf {|STR["%s"]|} s
  | EInt i -> Printf.sprintf "INT[%d]" i
  | EBool b -> Printf.sprintf "BOOL[%b]" b
  | EList es -> 
    let es = es |> List.map print_expr |> String.concat ", " in
    Printf.sprintf "LIST[%s]" es
  | ELet (name, value, body) -> 
    Printf.sprintf 
      "LET[%s -> %s,\n %s]"
       name (print_expr value) (print_expr body)
  | EFun (params, body) -> 
      Printf.sprintf 
        "FUN[(%s),\n%s]" 
        (params |> String.concat ", ")
        (print_expr body)
  | EIf (p, t, f) -> 
      Printf.sprintf 
        "IF[%s, %s, %s]" 
        (print_expr p)
        (print_expr t) 
        (print_expr f)
  | EApply (f, args) -> 
      Printf.sprintf 
        "APP[%s,(%s)]" 
          (print_expr f)
          (args |> List.map print_expr |> String.concat ",")

let print_toplevel t = 
  match t with 
  | TopDefine (name, body) -> 
     Printf.sprintf 
      "DEFINE [%s,%s]" name (print_expr body)
  | TopExpr e -> Printf.sprintf "EXPR[%s]" (print_expr e)