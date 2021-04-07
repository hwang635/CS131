(* CS131 Spring2021 HW1 *)

(*Q1. Write a fx subset a b that returns true iff (i.e., if and only if) a⊆b, 
i.e., iff the set represented by the list a is a subset of the set represented 
by the list b. Every set is a subset of itself. This function should be curried, 
and should be generic to lists of any type: that is, the type of subset should 
be a generalization of 'a list -> 'a list -> bool. *)
let rec subset a b = 
    match a with (*Empty list is subset, else check each a exists in b *)
    | [] -> true 
    | head :: rest -> 
        if List.exists (fun c -> head = c) b
        then subset rest b
        else false;;

(*Q2. Write a function equal_sets a b that returns true iff the 
represented sets are equal. *)
(* Equal if a is subset of b and b is subset of a *)
let equal_sets a b = (subset a b) && (subset b a);;

(* Q3. Write a function set_union a b that returns a list 
representing a∪b. *)
