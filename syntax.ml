type name = string 

type operator = OpAdd | OpMinus | OpTimes | OpDiv

type expr =
  | EString of string
  | EInteger of int
  | EBool of bool
  | EPair of expr * expr
  | EList of expr list
  | EOp of operator * expr * expr
  | ELet of name * expr * expr
  | ELambda of name * expr
  | EIf of expr * expr * expr
  | EApply of expr * expr list

type toplevel = TopDefine of name * expr | TopExpr of expr 