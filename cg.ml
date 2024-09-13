open Definitions

module Layout = Map.Make(String)

let cg_expr (env : int Layout.t ref) (e : expr) : X86.ins list * X86.operand =
  assert false

let cg_stmt (env : int Layout.t ref) (stmt : estmt) : X86.ins list =
match stmt with
| Input x -> assert false
| Val (x, y) -> assert false








let eprog_to_x86 (prog : eprog) : X86.prog =
  let env : int Layout.t ref = ref Layout.empty in
  let counter = ref 0 in
  assert false