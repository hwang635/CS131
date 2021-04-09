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
loop infintely in order to find a self member, which is not feasible.
Also, an OCaml list can only have 1 type. Since we are only dealing with
lists, a set list would need to contain 2 diff types (member type + set
type) in order to contain itself, which isn't possible. *)

(*Q6. Write a function computed_fixed_point eq f x that returns the 
computed fixed point for f with respect to x, assuming that eq is 
the equality predicate for f's domain. A common case is that eq will
be (=), that is, the builtin equality predicate of OCaml; but any 
predicate can be used. If there is no computed fixed point, your 
implementation can do whatever it wants. *)
(* Based off TA's hint code *)
let rec computed_fixed_point eq f x =
    if eq (f x) x
    then x
    else computed_fixed_point eq f (f x);; 

(*Q7. Write a function filter_reachable g that returns a copy of 
the grammar g with all unreachable rules removed. This function 
should preserve the order of rules: that is, all rules that are 
returned should be in the same order as the rules in g. *)
type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal;;

let isNonterm = function
    | N nonterminal -> true
    | T terminal -> false;;

(* Filter symbols to get list of nonterm symbols *)
let rec filterNonterm totalList =
    match totalList with
    | [] -> []
    | head::rest -> 
        match head with
            | N nonterm -> nonterm::(filterNonterm rest)
            | T term -> filterNonterm rest;;

(* Filter all rules to get nonterm symbols for rules *)
let rec getNonTermPerRules nonTermList rules = 
    match rules with
    | [] -> nonTermList
    | head:: rest ->
        if List.mem (fst head) nonTermList
        then
            let newList = filterNonterm (snd head) in
            getNonTermPerRules (set_union nonTermList newList) rest
        else getNonTermPerRules nonTermList rest;;
        
let rec getAllNontermRules nonTermList totalRules =
    let newList = getNonTermPerRules nonTermList totalRules in
    if equal_sets newList nonTermList
    then nonTermList
    else getNonTermPerRules newList totalRules;;

(* Use list.filter to filter out reachable rules from all rules *)
let filterReachableRules reachableSymbols rulesList =
    List.filter (fun rule -> List.mem (fst rule) reachableSymbols) rulesList;;

(* Based off TA hint code *)
let filter_reachable g =
    (* Separate start (first elem) from rules (second elem) *)
    let startSymbol = (fst g) in 
    let rules = (snd g) in
    (* Get reachable symbols *)
    let reachableSymbols = getAllNontermRules [startSymbol] rules in
    (* Filter rules *)
    (startSymbol, filterReachableRules reachableSymbols rules);;