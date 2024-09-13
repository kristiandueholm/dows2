type semant_error 
  = Undeclared of Definitions.varname 
  | Duplicate of Definitions.varname
  | Unused of Definitions.varname


type semant_result
  = Ok
  | Error of semant_error list
  | Warning of semant_error list
  
val semant: Definitions.eprog -> semant_result