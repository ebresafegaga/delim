(* CEK Machine *)
open Ast

type value = 
  | VString of string 
  | VInt of int 
  | VBool of bool 
  | VList of value list 
  | VFun of env * Syntax.name list * Syntax.expr

and env = (Syntax.name * value) list

type kont = 
  | KDone
  | KLet of env * Syntax.expr * kont 
  | KList of env * Syntax.expr list * kont 
  | KIf of env * Syntax.expr * Syntax.expr * kont 
  | KApply of env * Syntax.expr list * kont 

type machine = Syntax.expr * env * kont