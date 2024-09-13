open Definitions

(* Task 2 *)

(* -- Use this in your solution without modifications --  *)
type semant_error 
  = Undeclared of varname 
  | Duplicate of varname
  | Unused of varname


type semant_result
  = Ok
  | Error of semant_error list
  | Warning of semant_error list

(* My solution *)

(* Make a Map for variables as keys, bools as values. If var. has been used
bool is set to true, unused false (default) *)

module Variable = struct
  type t = varname
  let compare = String.compare
end

module VarMap = Map.Make(Variable)

let union_varmap m1 m2 =
  VarMap.union (fun _key val1 val2 ->
    if val1 || val2 then Some true else Some false
  ) m1 m2

(* let rec expression_checker (variables:bool VarMap.t) = function
(* Takes VarMap and expression, returns list of semantic errors. *)
| Var x ->
    (match VarMap.find_opt x variables with
    | Some _ -> []
    | None -> [Undeclared x])
| BinOp (_, left, right) ->
    let left_errors = expression_checker variables left in
    let right_errors = expression_checker variables right in
    left_errors @ right_errors
| Int _ -> [] *)

(* Expression checker glory *)
let rec expression_checker (variables:bool VarMap.t) = function
(* Takes VarMap and expression, returns list of semantic errors. *)
| Var x ->
    (match VarMap.find_opt x variables with
    | Some _ ->
        (* Variable is in map, update to say it is used *)
        let variables_updated = VarMap.add x true variables in
        variables_updated, []
    | None -> variables, [Undeclared x])
| BinOp (_, left, right) ->
    let left_vars, left_errors = expression_checker variables left in
    let right_vars, right_errors = expression_checker variables right in
    let combined_vars = union_varmap left_vars right_vars in
    combined_vars, (left_errors @ right_errors)
| Int _ -> variables, []

(* First iteration of statement checker *)
(* let estmt_checker (variables:bool VarMap.t) = function
| Input v ->
  (* Check if v unbound, if unbound add to variables, if bound give error *)
  (match VarMap.find_opt v variables with
  | None ->
      let variables_new = VarMap.add v false variables in
      variables_new, []
  | Some _ -> variables, [Duplicate v])
| Val (v, e) ->
  (* Check expression first, then for unbounds *)
  let e_errors = expression_checker variables e in
  (match VarMap.find_opt v variables with
  | None ->
      let variables_new = VarMap.add v false variables in
      variables_new, e_errors
  | Some _ -> variables, Duplicate v :: e_errors) *)

(* Second refactored interation of statement checker *)
let estmt_checker (variables:bool VarMap.t) = function
| Input v | Val (v, _) as s ->
  (* When Val, expression should be checked first *)
  let variables, e_errors = match s with
    | Val (_, e) -> expression_checker variables e
    | Input _ -> variables, [] in

  (* Seeing if varname is in varmap *)
  (* If not, add it. If it is give error *)
  match VarMap.find_opt v variables with
  | None ->
      let variables_new = VarMap.add v false variables in
      variables_new, e_errors
  | Some _ -> variables, Duplicate v :: e_errors

(* Goes through estmt list until empty, then checks return expr *)
(* After checking return expression, it checks map for any false values *)
(* False values indicate unused variables. These are added to error list *)
let rec semant_helper variables = function
| s :: tl, e ->
    let variables_new, stmt_errors = estmt_checker variables s in
    stmt_errors @ (semant_helper variables_new (tl, e))
| [], e ->
    let variables_new, errors = expression_checker variables e in
    let unused_vars_errors = VarMap.fold (fun name v acc ->
      if not v then Unused name :: acc else acc
    ) variables_new [] in
    errors @ unused_vars_errors


let semant prog = 
  let variables : bool VarMap.t = VarMap.empty in
  let errors = semant_helper variables prog in
  if List.is_empty errors then Ok else Error errors