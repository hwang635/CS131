kenken_testcase(
  6,
  [
   +(11, [[1|1], [2|1]]),
   /(2, [1|2], [1|3]),
   *(20, [[1|4], [2|4]]),
   *(6, [[1|5], [1|6], [2|6], [3|6]]),
   -(3, [2|2], [2|3]),
   /(3, [2|5], [3|5]),
   *(240, [[3|1], [3|2], [4|1], [4|2]]),
   *(6, [[3|3], [3|4]]),
   *(6, [[4|3], [5|3]]),
   +(7, [[4|4], [5|4], [5|5]]),
   *(30, [[4|5], [4|6]]),
   *(6, [[5|1], [5|2]]),
   +(9, [[5|6], [6|6]]),
   +(8, [[6|1], [6|2], [6|3]]),
   /(2, [6|4], [6|5])
  ]
).

/* Transpose code from TA hint code 
https://stackoverflow.com/questions/4280986/how-to-transpose-a-matrix-in-prolog */
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).
transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).
lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

/* FINITE DOMAIN SOLVER */
/* Check each row has correct length N + unique #s 1-N */
validRows(N, []).
validRows(N, [CurrentRow|Rest]) :-
    length(CurrentRow, N),
    fd_all_different(CurrentRow),
    fd_domain(CurrentRow, 1, N),
    validRows(N, Rest).

/* Access elem @ this row + col position */
getElem(Elem, [RowPos|ColPos], T) :-
    nth(RowPos, T, Row),
    nth(ColPos, Row, Elem).

/* Get #s @ this pos, perform the op, + check correctness */
checkAdd(0, [], T).
checkAdd(Sum, [Pos1|PosRest], T) :-
    getElem(Elem, Pos1, T),
    checkAdd(SumRest, PosRest, T),
    Sum #= Elem + SumRest.

checkMult(1, [], T).
checkMult(Prod, [Pos1|PosRest], T) :-
    getElem(Elem, Pos1, T),
    checkMult(ProdRest, PosRest, T),
    Prod #= Elem * ProdRest.

/* Pattern match for which op, check or call rec fx to check result */
matchOp(+(Sum, Ints), T) :-
    checkAdd(Sum, Ints, T).
matchOp(-(Diff, Int1, Int2), T) :-
    getElem(Elem1, Int1, T), getElem(Elem2, Int2, T),
    (Diff #= Elem1 - Elem2 ; Diff #= Elem2 - Elem1).
matchOp(*(Prod, IntList), T) :-
    checkMult(Prod, IntList, T).
matchOp(/(Quot, Int1, Int2), T) :-
    getElem(Elem1, Int1, T), getElem(Elem2, Int2, T),
    (Quot #= Elem1/Elem2 ; Quot #= Elem2/Elem1).

/* Check all cage constraints are satisfied */
checkCages([], T).
checkCages([Head|Rest], T) :-
    matchOp(Head, T),
    checkCages(Rest, T).

/* validRowsCol(N, T) :-
	validRows(N, T),
	validCol(N, T). */

/* Takes in N size, C cages, T list of ints rep grid */
/* Valid if size NxN and valid rows + cols (check cols by transposing, then checking
rows of transposed matrix). */
kenken(N, C, T) :-
    length(T, N),
    validRows(N, T),
	/* Transpose matrix to check transposed col same way as rows */
	transpose(T, TranposeT),
    validRows(N, TranposeT), 
	checkCages(C, T),
    maplist(fd_labeling, T).

/* PLAIN SOLVER */

/* Creates finite domain from 1 to N 
https://stackoverflow.com/questions/27151274/prolog-take-the-first-n-elements-of-a-list */
plainCreateDomain(N, Domain) :- 
	findall(Num, between(1, N, Num), Domain).

/* Check each row has correct length N + in range of Domain */
plainWithinDomain([], Domain).
plainWithinDomain([RowHead|RowRest], Domain) :-
	member(RowHead, Domain),
	plainWithinDomain(RowRest, Domain).

/* Check if all #s in row = unique by sorting
https://stackoverflow.com/questions/20131904/check-if-all-numbers-in-a-list-are-different-in-prolog */
plainAllDiff(Row) :-
    sort(Row, SortedRow),
    length(Row, OrigRowLength),
    length(SortedRow, SortedRowLength),
    OrigRowLength == SortedRowLength.

/* Check length of row, create domain 1-N, check each row has unique #s 1-N */
plainValidRows(N, []).
plainValidRows(N, [CurrentRow|Rest]) :-
    length(CurrentRow, N),
	plainCreateDomain(N, Domain),
	plainWithinDomain(CurrentRow, Domain),
	plainAllDiff(CurrentRow),
    plainValidRows(N, Rest).

plain_kenken(N, C, T) :-
	length(T, N),
	plainValidRows(N, T),
	transpose(T, TranposeT),
	plainValidRows(N, TranposeT),
	checkCages(C, T).

/* I tested performance with the statistics function on given testcase
kenken(
4,
  [
   +(6, [[1|1], [1|2], [2|1]]),
   *(96, [[1|3], [1|4], [2|2], [2|3], [2|4]]),
   -(1, [3|1], [3|2]),
   -(1, [4|1], [4|2]),
   +(8, [[3|3], [4|3], [4|4]]),
   *(2, [[3|4]])
  ],
  T
), write(T), nl, fail. 

As the stastistic data below shows, the plain solver is significantly slower
than the finite domain solver, more than twice as slow. The memory usage is
about the same, but the plain solver is significantly slower than the finite
domain solver for all testcases.

FINITE DOMAIN SOLVER: 
Memory               limit         in use            free
   trail  stack      16383 Kb            0 Kb        16383 Kb
   cstr   stack      16384 Kb            0 Kb        16384 Kb
   global stack      32767 Kb            3 Kb        32764 Kb
   local  stack      16383 Kb            0 Kb        16383 Kb
   atom   table      32768 atoms      1792 atoms     30976 atoms

Times              since start      since last
   user   time       0.012 sec       0.002 sec
   system time       0.009 sec       0.003 sec
   cpu    time       0.021 sec       0.005 sec
   real   time      63.863 sec      46.676 sec

PLAIN SOLVER:
Memory               limit         in use            free
   trail  stack      16383 Kb            0 Kb        16383 Kb
   cstr   stack      16384 Kb            0 Kb        16384 Kb
   global stack      32767 Kb            3 Kb        32764 Kb
   local  stack      16383 Kb            0 Kb        16383 Kb
   atom   table      32768 atoms      1792 atoms     30976 atoms

Times              since start      since last
   user   time       0.015 sec       0.003 sec
   system time       0.012 sec       0.003 sec
   cpu    time       0.027 sec       0.006 sec
   real   time     150.123 sec      86.260 sec
*/
