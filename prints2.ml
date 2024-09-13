open Definitions
open Semantics
open Printer

let eprog_01: eprog = (
      [ Input "x" ; Val ("y", BinOp (Add, Var "x", Int 1)) ],
      BinOp (Add, Var "x", Var "y"))

let eprog_02: eprog = (
      [ Input "x" ; Val ("y", BinOp (Add, Var "x", Int 1)) ],
      BinOp (Add, Var "x", Var "z"))

let eprog_03: eprog = (
      [ Input "x" ; Val ("y", BinOp (Add, Var "x", Int 1)); Input "x"],
      BinOp (Add, Var "x", Var "z"))

let eprog_list = [eprog_01; eprog_02; eprog_03]
let eprog_strings = List.map (fun x -> string_of_eprog x) eprog_list
let eprog_errors = List.map (fun x -> semant x) eprog_list

let () = List.iter (Printf.printf "%s\n\n") eprog_strings

let ()  = Printf.printf "Eprog1 evaluates to %d\n" (Evaluator.eval eprog_01)