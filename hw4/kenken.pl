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

/* Check each row has correct length N + unique #s 1-9 */
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

/* Get # @ this pos, perform the op, + check correctness */
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
    getElem(Elem1, Int1, T),
    getElem(Elem2, Int2, T),
    (Diff #= Elem1 - Elem2 ; Diff #= Elem2 - Elem1).
matchOp(*(Prod, IntList), T) :-
    checkMult(Prod, IntList, T).
matchOp(/(Quot, Int1, Int2), T) :-
    getElem(Elem1, Int1, T),
    getElem(Elem2, Int2, T),
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