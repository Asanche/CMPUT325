%! Question 1
xreverse([], []).
xreverse([H|T], R):-
    xreverse(T, RT),
    append(RT, [H], R).