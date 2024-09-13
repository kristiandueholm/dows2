open Definitions

(* Task 1 *)

let string_of_binop b = match b with
| Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"

let rec string_of_expr e = match e with
| Int x -> string_of_int x
| BinOp (x, y, z) ->
    Printf.sprintf "(%s %s %s)" (string_of_expr y) (string_of_binop x) (string_of_expr z)
| Var x -> x

let rec string_of_eprog prog =
  let open Printf in
  match prog with
    | [], x -> sprintf "return %s" (string_of_expr x)
    | Val (x, y) :: xs, z -> 
        sprintf "val %s = %s\n" x (string_of_expr y) ^ string_of_eprog (xs, z)
    | Input x :: xs, z ->
        sprintf "input %s\n" x ^ string_of_eprog (xs, z)