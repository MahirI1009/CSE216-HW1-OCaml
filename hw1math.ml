(*2.2 Definition of abstract syntax of a simple arithmetic language*)
type expr = 
  | Const of int
  | Var of string
  | Plus of args
  | Minus of args
  | Mult of args
  | Div of args
and args = { arg1 : expr; arg2 : expr };;

(*2.3 evaluate function, which takes a single type expr defined above, and evaluates the expr*)
let rec evaluate arith_expr = 
  match arith_expr with
  | Const i -> i
  | Var x -> failwith "This function does not work with type Var"
  | Plus {arg1; arg2} -> evaluate arg1 + evaluate arg2
  | Minus {arg1; arg2} -> evaluate arg1 - evaluate arg2
  | Mult {arg1; arg2} -> evaluate arg1 * evaluate arg2
  | Div {arg1; arg2} -> evaluate arg1 / evaluate arg2;;