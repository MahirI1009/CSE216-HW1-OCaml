(*#1: pow and float_pow functions*)
let rec pow x n = 
  if n = 0 then 1
  else if n = 1 then x
  else x * pow x (n-1);;

let rec float_pow x n = 
  if n = 0 then 1.
  else if n = 1 then x
  else x *. (float_pow x (n-1));;

(*#2: compress function, removes consecutive duplicates from list*)
let rec compress list = 
  match list with
  | [] -> []
  | [x] -> [x]
  | h::(h2::_ as t) -> if h = h2 then compress t
    else h::(compress t);; 

(*#3 remove_if function, remove element if it satisfies condition*)

let rec remove_if list pred =
  match list with
  | [] -> []
  | h::t -> if (pred h) then (remove_if t pred)
    else h::(remove_if t pred);;

(*#4 slice function, returns sublist from index i to index j, uses 2 helper functions*)
let rec slice_i list i =
  match list with 
  | [] -> []
  | [x] -> [x]
  | h::t -> if i > 0 then slice_i t (i-1)
    else (h::t);;

let rec slice_j list i j =
  match list with
  | [] -> []
  | [x] -> [x]
  | h::t -> if i > j+1 then []
    else if j > 0 then h::(slice_j t i (j-1))
    else (h::t);;

let slice list i j  = 
    if i > j then []
    else (slice_j (slice_i list i) i (j-i));;

(*#5 equivs function, partitions list into equivalence classes*)
let rec equiv_false equivclass x list =
  match list with
  | [] -> []
  | h::t -> if (equivclass h x) = false then h::(equiv_false equivclass x t) else (equiv_false equivclass x t);;

let rec equiv_true equivclass x list =
  match list with
  | [] -> []
  | h::t -> if (equivclass h x) then h::(equiv_true equivclass x t) else (equiv_true equivclass x t);;
  
let rec equivs equivclass list =
  match list with 
  | [] -> []
  | h::t -> (equiv_true equivclass h list) :: equivs equivclass (equiv_false equivclass h list);;
  
(*#6 goldbachpair function, finds two prime numbers that add up to a given int*)
let rec is_prime p i =
  if p mod i = 0 && p != i then false 
  else if p mod i != 0 && i = 2 then true
  else is_prime p (i-1);;

let rec find_sum x y = 
  if is_prime x x && is_prime (y-x) (y-x) then (x, y-x)
  else find_sum (x+1) y;;

let goldbachpair z = 
  find_sum 2 z;;

(*#7 equiv_on function, finds if 2 functions f and g have the same behavior on a given list*)
let eval f g x =
  if (f x) = (g x) then true
  else false;;

let rec equiv_on f g list =
  match list with
  | [] -> true
  | [x] -> eval f g x
  | h::t -> if (eval f g h) then equiv_on f g t 
    else false;;

(*#8 pairwisefilter function, cmp function compares 2 elements from a list, returns 1 of the elements, makes a list of returned elements*)
let rec pairwisefilter cmp list =
  match list with
  | [] -> []
  | [x] -> [x]
  | h::h2::t -> if (cmp h h2) = h then h::(pairwisefilter cmp t)
    else h2::(pairwisefilter cmp t);;

(*#9 polynomial function, takes list of tuples, turns it into  polynomial function *)
let rec polynomial list v =
  match list with
  | [] -> 0
  | (c, e)::t -> (c * (pow v e)) + polynomial t v;;

(*#10 powerset function, takes in a list (which is treated as a set) and returns it's powerset*)

let rec partition func list =
  match list with 
  | [] -> []
  | h::t -> func h :: (partition func t)
  
let rec powerset list = 
  match list with
  | [] -> [[]]
  | h::t -> let new_list = powerset t in new_list @ (partition (fun x -> h :: x) new_list);;