(* CS131 Spring2021 HW2 *)

type ('nonterminal, 'terminal) symbol = 
    | N of 'nonterminal
    | T of 'terminal;;

type ('nonterminal, 'terminal) parse_tree =
    | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
    | Leaf of 'terminal;;

(* Warm-up. Write a function convert_grammar gram1 that returns a 
Homework 2-style grammar, which is converted from the Homework 1-style
grammar gram1. Test your implementation of convert_grammar on the 
test grammars given in Homework 1. For example, the top-level 
definition let awksub_grammar_2 = convert_grammar awksub_grammar 
should bind awksub_grammar_2 to a Homework 2-style grammar that is 
equivalent to the Homework 1-style grammar awksub_grammar. *)
(* Production fx takes in nonterm + returns list of all rules RHS
that correspond to that nonterm *)
let rec prodFx nonterm rulesList =
    match rulesList with
    | [] -> [] (*Empty list, done! *)
    | head::rest -> 
        (* If start symbol = nonterm, concat RHS to rulesList *)
        let start = (fst head) in
        if start = nonterm then [snd head]@(prodFx nonterm rest)
        else prodFx nonterm rest;;
    
(* Convert HW1 grammar + ret HW2 grammar *)
let convert_grammar gram1 = 
    (* Separate start (first elem) from rules (second elem) *)
    let start = (fst gram1) in
    let rulesList = (snd gram1) in
    (start, function nonterm -> (prodFx nonterm rulesList));;