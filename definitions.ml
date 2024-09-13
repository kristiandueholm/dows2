(* -- Use this in your solution without modifications *)
(* Defining the type for binary operations *)
type binop =
  | Add | Sub | Mul | Div

type varname = string                (* variable names are strings *)

(* Defining the type for arithmetic expressions *)
type expr =
  | Int of int                       (* Integer constant *)
  | BinOp of binop * expr * expr     (* Binary operation *)
  | Var of varname                   (* Variable lookup  *)


(* Defining the type of statements *)  
type estmt = 
  | Val of varname * expr            (* Binding variable to a value *)
  | Input of varname                 (* Input statement *)

(* Expression program is a list of statements 
   followed by an expression *) 
type eprog = estmt list * expr