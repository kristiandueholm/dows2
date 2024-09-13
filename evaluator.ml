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
| [] -> variables
| Val (var, e1) :: tl ->
    let variables_new = VarMap.add var (eval_expr variables e1) variables in
    eval_helper variables_new tl
| Input var :: tl ->
    let user_input = () |> eprog_input in
    let variables_new = VarMap.add var user_input variables in
    eval_helper variables_new tl

let eval prog = 
  let variables : int VarMap.t = VarMap.empty in
  let (statements, return_expr) = prog in
  let variables_new = eval_helper variables statements in
  eval_expr variables_new return_expr