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