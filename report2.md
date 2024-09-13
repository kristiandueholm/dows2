**Task 1: Pretty printer for expression programs**

I did this task by extending last weeks work as recommended. I used my `string_of_expr` to turn expressions into strings, together with a `string_of_binop`. The version I used here was the non-glory for simplicity, so there are parenthesis around every expression.

I am able to match the `eprog` with 3 cases: The statement list is empty which means return, head of statement list is Val, so give the varname followed by expression, lastly it can be Input, where it is simply input followed by varname. These are concatted together with newlines.