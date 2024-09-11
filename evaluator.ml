open Definitions

module Variable = struct
  type t = varname
  let compare = String.compare
end

module VarMap = Map.Make(Variable)

let eprog_input() 
= Printf.printf "Please enter an integer: " ; read_line () |> int_of_string

let op_of_binop = function
| Add -> ( + )
| Sub -> ( - )
| Mul -> ( * )
| Div -> ( / )

let rec eval_expr variables = function
| Int x -> x
| Var x -> VarMap.find x variables
| BinOp (x, y, z) -> op_of_binop x (eval_expr variables y) (eval_expr variables z)

let rec eval_helper variables = function
| [], e -> eval_expr variables e
| Val (var, e1) :: tl, e2 -> assert false
| Input var :: tl, e ->

let eval prog = 
  let variables : int VarMap.t = VarMap.empty in
  eval_helper variables prog