(* From hw1, for testing warmup *)
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
(* parse_tree_leaves tree = sentence *)

(* Make matcher *)