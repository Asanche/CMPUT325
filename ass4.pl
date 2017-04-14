count(N, N, N).
count(N0, N1, N) :-
   N1 > N0,
   N = N1;
   N2 is N1-1,
   count(N0, N2, N).