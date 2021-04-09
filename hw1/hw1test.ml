(* Tests for q1 subset, sb true *)
let my_subset_test0 = subset [2; 1] [1;2;3]
let my_subset_test1 = subset ["hello"; "world"] ["hello"; "small"; "world"]
let my_subset_test2 = (subset [1;3;7] [7;1;3])
let my_subset_test3 = not (subset [3;1;3] [])
let my_subset_test4 = not (subset ["eggert"] ["hello"; "world"])

(* Tests for q2 equal_sets, sb true *)
let my_equal_sets_test0 = not (equal_sets [] [[]])
let my_equal_sets_test1 = not (equal_sets ["hello"; "world"; "hi"] ["hello"; "hello"])

(*Tests for q3 set_union, sb true *)
let my_set_union_test0 = equal_sets(set_union ["a";"b";"c"] ["d"]) ["a";"b";"c";"d"]
let my_set_union_test1 = equal_sets(set_union [5;4;3;3;3;2] [1;3;3;3]) [1;2;3;4;5]

(*Tests for q4 set_all_union, sb true *)
let my_set_all_union_test0 =
    equal_sets (set_all_union [[1]; [2]; [2]; [2]; [3]; [4]]) [1;2;3;4]
let my_set_all_union_test1 =
    equal_sets (set_all_union [[3;2;1]; [1;2;3]; [2;2;2]]) [1;2;3]
let my_set_all_union_test2 =
    equal_sets (set_all_union [["a"; "b"]; ["cat"]]) ["a";"b";"cat"]

(*Tests for q6 computed_fixed_point*)
let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x /. 100. ) 1000000000. = 0.
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. x) 5. = infinity

(*Tests for q7 *)
type my_nonterminals0 =
  | Cat | Dog | Mouse

let my_grammar0 = [
  Cat, [T "orange"];
  Cat, [T "black"];
  Dog, [N Dog];
  Cat, [T "round"];
  Mouse, [T "cheesy"];
  Mouse, [N Cat; N Dog];
]

let my_test_grammar0 = Cat, my_grammar0
let my_filter_reachable_test0 = filter_reachable my_test_grammar0 
  = (Cat, [(Cat, [T "orange"]); (Cat, [T "black"]); (Cat, [T "round"])])

let my_test_grammar1 = Dog, my_grammar0
let my_filter_reachable_test1 = filter_reachable my_test_grammar1
  = (Dog, [(Dog, [N Dog])])
