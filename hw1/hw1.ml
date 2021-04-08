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
(* Concat a + b then call sort_uniq to sort + remove duplicates *)
let set_union a b = List.sort_uniq compare (a@b);;

(* Q4. Write a function set_all_union a that returns a list 
representing ∪[x∈a]x, i.e., the union of all the members of 
the set a; a should represent a set of sets. *)
(* Go through each set in a + call set_union, using fold_left *)
let set_all_union a = 
    match a with
    | [] -> []
    | head :: rest -> List.fold_left set_union head rest;;

(*Q5. It's not possible to write a fx self_member that identifies if
a set is a member of itself. The only way for a set to be a member of
itself is for the set to have a member that is the set containing
itself which also needs to contain itself and so on, creating an
infinite set. A function checking for this would therefore have to
loop infintely in order to find a self member, which is not feasible *)

(*Q6. Write a function computed_fixed_point eq f x that returns the 
computed fixed point for f with respect to x, assuming that eq is 
the equality predicate for f's domain. A common case is that eq will
be (=), that is, the builtin equality predicate of OCaml; but any 
predicate can be used. If there is no computed fixed point, your 
implementation can do whatever it wants. *)
(* Based off TA's hint code *)
let rec computed_fixed_point eq f x =
    if eq (fun f x -> f x) x
    then x
    else computed_fixed_point eq f (fun f x -> f x);; 
    
    

