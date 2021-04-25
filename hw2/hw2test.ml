(* From hw1, for testing warmup
type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = Expr, awksub_rules

(* For parse_tree *)
type awksub_nonterminals =
  | Expr | Term | Lvalue | Incrop | Binop | Num
(* This is an example of expression tree, but it's not a parsing tree *)
let expr_tree = Node ("+", [Leaf 3; Node ("*", [Leaf 4; Leaf 5])])
(* val expr_tree : (string, int) parse_tree = ... *)

(* An sentence of awk grammar *)
let sentence = ["9"; "+"; "$"; "1"]

(* The parsing tree of it *)
let tree = Node (Expr, 
                  [Node (Term, [Node (Num, [Leaf "9"])]);
                   Node (Binop, [Leaf "+"]);
                   Node (Expr, [Node (Term, [Node (Lvalue, 
                                                    [Leaf "$";
                                                     Node (Expr,
                                                            [Node (Term, [Node (Num, [Leaf "1"])])])])])])])
parse_tree_leaves tree = sentence *)

(* Make matcher testing *)
let accept_all string = Some string (* From hw2sample *)
let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type myNonterms = 
  | Noun | Adj | Adv | Verb | Article | Phrase

let prodFx = function
  | Phrase -> 
      [[N Noun; N Verb];
      [N Adj; N Noun; N Verb];
      [N Noun; N Article; N Noun];
      [N Adj; N Noun; N Adv; N Verb];]
  | Noun ->
    [[T "student"]; [T "cat"]; [T "dog"]]
  | Adj ->
    [[T "smart"]; [T "lazy"]; [T "sleepy"]; [T "tasty"]]
  | Adv ->
    [[T "sadly"]; [T "slowly"]]
  | Verb ->
    [[T "programs"]; [T "sleeps"]; [T "cooks"]]
  | Article ->
      [[T "the"]; [T "of"]; [T "a"]; [T "for"]]

let myGrammar = (Phrase, prodFx)

let frag1 = ["lazy"; "cat"; "sadly"; "programs"]
let frag2 = ["dog"; "happily"; "programs";]

let make_matcher_test0 = ((make_matcher myGrammar accept_empty_suffix frag1) = Some [])
let make_matcher_test1 = ((make_matcher myGrammar accept_empty_suffix frag2) = None)
