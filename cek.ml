(* CEK Machine *)

type value = 
  | VString of string 
  | VInt of int 
  | VBool of bool 
  | VList of value list 
  | VFun of env * Syntax.name list * Syntax.expr

and env = (Syntax.name * value) list

type kont = 
  | KDone
  | KLet of Syntax.name * value * kont 

type machine = Syntax.expr * env * kont 

let step (exp, env, cont) = 
  failwith ""