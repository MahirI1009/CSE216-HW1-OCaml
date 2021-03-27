(*2.1, truth_table function, copy-pasted the bool-expr type from the hw1.pdf file to make the function work*)
type bool_expr =
| Lit of string
| Not of bool_expr
| And of bool_expr * bool_expr
| Or of bool_expr * bool_expr;;

let rec truth_values p bool1 q bool2 expr = 
  match expr with
  | Lit x -> if x = p then bool1
    else if x = q then bool2
    else false
  | Not r -> not(truth_values p bool1 q bool2 r)
  | And (r,s) -> (truth_values p bool1 q bool2 r) && (truth_values p bool1 q bool2 s)
  | Or(r,s) -> (truth_values p bool1 q bool2 r) || (truth_values p bool1 q bool2 s);;

let truth_table a b expr =
  [(true, true, (truth_values a true b true expr)); 
   (true, false, (truth_values a true b false expr)); 
   (false, true, (truth_values a false b true expr)); 
   (false, false, (truth_values a false b false expr))];;