type name = string 

type operator = OpAdd | OpMinus | OpTimes | OpDiv

type expr = 
  | EString of string 
  | EInteger of int
  | EBool of bool
  | EPair of expr * expr 
  | EOp of operator * expr * expr
  | ELet of name * expr * expr 
  | ELambda of name * expr

type toplevel = TopDefine of name * expr | TopExpr of expr 