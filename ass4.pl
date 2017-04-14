/*
    Assignment # 4
    Author: Adam Sanche
    Date: April 13, 2017
    Student ID: 1393331
    Class: CMPUT 325
*/

/* Link the Constraint Programming library clpfd */
:- use_module(library(clpfd)).

/* QUESTION 1
    This predicate simply specifies the condition for four squares, then
    specifies the order from Greatest to least that the squares should be
    applied to S1, ..., S4.
    It searches for the squares in the full range of N. This likely is not perfectly
    efficient. e.g. any number aboe four will have no squares bigger than half of 
    it's value. */
fourSquares(N, [S1, S2, S3, S4]):-
    Vars = [S1, S2, S3, S4],
    (S1*S1 + S2*S2 + S3*S3 + S4*S4) #= N,
    S1 in 0..N,
    S2 in 0..N,
    S3 in 0..N,
    S4 in 0..N,
    S1 #=< S2,
    S2 #=< S3,
    S3 #=< S4,
    label(Vars).

/* QUESTION 2
    This predicate simply redirects to the other three predicates listed below.
    It starts with the largest previous value being 0, as nothing has been disarmed. */
disarm(Adivisions, Bdivisions, Solutions) :- disarm(Adivisions, Bdivisions, Solutions, 0).

/* This predicate ensures that the Solution output is given in the correct form, and that
    multiple solutions are given to the problem as required. */
disarm([], [], Solution, _) :- Solution = [].

/* This is the predicate that is responsible for adding a solution for the case 
    where Adivisions should have one divisions disarmed, and two B divisions should
    be disarmed. This is simply determined to be used if the potential Disarming candidate
    for A is greater than the previous largest division disarmed. */
disarm(Adivisions, Bdivisions, Solution, PrevLargest):-
    Vars = [DisA, DisB1, DisB1],
    select(DisA, Adivisions, DisarmedA),
    select(DisB1, Bdivisions, TempDisarmedB),
    select(DisB2, TempDisarmedB, DisarmedB),
    DisB1 #=< DisB2,
    DisA #= DisB1 + DisB2,
    PrevLargest #=< DisA,
    disarm(DisarmedA, DisarmedB, TempSolution, DisA),
    append([[[DisA], [DisB1, DisB2]]], TempSolution, Solution),
    label(Vars).

/* This is the predicate responsible for disarming two B divisions and one
    A division. It determines it should use this predicate if the potential candidate
    for disarming in B is larger than the previous largest value disarmed. */
disarm(Adivisions, Bdivisions, Solution, PrevLargest):-
    Vars = [DisA1, DisA2, DisB],
    select(DisB, Bdivisions, DisarmedB),
    select(DisA1, Adivisions, TempDisarmedA),
    select(DisA2, TempDisarmedA, DisarmedA),
    DisA1 #=< DisA2,
    DisB #= DisA1 + DisA2,
    PrevLargest #=< DisB,
    disarm(DisarmedA, DisarmedB, TempSolution, DisB),
    append([[[DisA1, DisA2], [DisB]]], TempSolution, Solution),
    label(Vars).


