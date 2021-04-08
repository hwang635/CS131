(* Tests for q1 subset, sb true *)
let subset_test0 = subset [] [1;2;3]
let subset_test1 = subset [3;1;3] [1;2;3]
let subset_test2 = not (subset [1;3;7] [4;1;3])
let my_subset_test0 = subset [2; 1] [1;2;3]
let my_subset_test1 = subset ["hello"; "world"] ["hello"; "small"; "world"]
(*Sb false *)
let my_subset_test2 = not (subset [1;3;7] [7;1;3])
let my_subset_test3 = subset [3;1;3] []
let my_subset_test4 = subset ["eggert"] ["hello"; "world"]

(* Tests for q2 equal_sets, sb true *)
let equal_sets_test0 = equal_sets [1;3] [3;1;3]
let equal_sets_test1 = not (equal_sets [1;3;4] [3;1;3])
(* Sb false *)
let my_equal_sets_test0 = equal_sets [] [[]]
let my_equal_sets_test1 = equal_sets ["hello"; "world"; "hi"] ["hello"; "hello"]

(*Tests for q3 set_union, sb true *)
let set_union_test0 = equal_sets (set_union [] [1;2;3]) [1;2;3]
let set_union_test1 = equal_sets (set_union [3;1;3] [1;2;3]) [1;2;3]
let set_union_test2 = equal_sets (set_union [] []) []
let my_set_union_test0 = equal_sets(set_union ["a";"b";"c"] ["d"]) ["a";"b";"c";"d"]
let my_set_union_test1 = equal_sets(set_union [5;4;3;3;3;2] [1;3;3;3]) [1;2;3;4;5]

(*Tests for q4 set_all_union, sb true *)
let set_all_union_test0 =
  equal_sets (set_all_union []) []
let set_all_union_test1 =
  equal_sets (set_all_union [[3;1;3]; [4]; [1;2;3]]) [1;2;3;4]
let set_all_union_test2 =
  equal_sets (set_all_union [[5;2]; []; [5;2]; [3;5;7]]) [2;3;5;7]
let my_set_all_union_test0 =
    equal_sets (set_all_union [[1]; [2]; [2]; [2]; [3]; [4]]) [1;2;3;4]
let my_set_all_union_test1 =
    equal_sets (set_all_union [[3;2;1]; [1;2;3]; [2;2;2]]) [1;2;3]
let my_set_all_union_test2 =
    equal_sets (set_all_union [["a"; "b"]; ["cat"]]) ["a";"b";"cat"]