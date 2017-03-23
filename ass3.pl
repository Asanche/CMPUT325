/** Question 1
 * This predicate simply appends the reverse of the tail with the head
 */
xreverse([], []).
xreverse([H | T], R):-
    xreverse(T, RT),
        append(RT, [H], R).

/** Question 2
 * This predicate reverses the list, removed the duplicates, then reverses
 * the list again. Reverse makes it remove duplicates from right to left
 * I could have used the built in reverse/2 instead of my xreverse/2 here.
 */
xunique([], []).
xunique(L, O):-
    xreverse(L, R), 
        xremovedupes(R, D), 
        xreverse(D, O).

/**
 * Removes duplicates from a list. Simply recursivelt checks if the head is a 
 * member of the tail. If it is, recurs on tail, if it isn't, it appends the 
 * head to the recursion on the tail.
 */
xremovedupes([], []).
xremovedupes([H | T], O):-
    member(H, T) -> xremovedupes(T, O);
    xremovedupes(T, S), 
        append([H], S, O).

/** Question 3
 * This predicate returns the elements that are unique between two lists.
 * It checks if the list is empty, as [] doesnt unify with [H | T] then
 * splits L1 into H and T. It checks if H is in L2. If it is, it recurs on
 * the tail. If it isn't, it recurrs on tail, and appends the Head to the front
 * of the result of the recursion.
 */
xdiff([], []).
xdiff(L1, L2, L):-
    L1 == [] -> L = [];
    L1 = [H | T],
        member(H, L2) -> xfindunique(T, L2, L);
    L1 = [H | T],
        xfindunique(T, L2, S), 
        append([H], S, L).

/** Question 4
 * This preducate simply reverses the list, then returns the head as the last
 * element and the tail as the rest. ezpz.
 */
removeLast(L, L1, Last):-
    xreverse(L, [H | T]), 
        Last = H, 
        L1 = T.

/* Question 5 */
/* Sample Graph */
node(a).
node(b).
node(c).
node(d).
node(e).

edge(a,b).
edge(b,c).
edge(c,a).
edge(d,a).
edge(a,e).

/* Provided Predicates */
clique(L) :- findall(X,node(X),Nodes), xsubset(L,Nodes), allConnected(L).
xsubset([], _).
xsubset([X|Xs], Set) :- xappend(_, [X|Set1], Set), xsubset(Xs, Set1).
xappend([], L, L).
xappend([H|T], L, [H|R]) :- xappend(T, L, R).

/** Question 5.1
 * This predicate checks that all of the nodes in a list are connected to eachother. 
 * It returns true on empty list. It then unifies the list to 
 * head and tail, then checks if the head is connected to all of the Nodes
 * in the rest of the list. It recurs with the tail, as it won't have to 
 * check the current head anymore.
 */
allConnected([]).
allConnected(L):-
    L == [] -> true;
    L = [H | T],
        connected(H, T) -> allConnected(T).

/* This predicate checks if a node is connected to all the nodes in the list.
 * First it gets all of the nodes that the head is connected to by finding all
 * edges with the Head in either the first or second position. It then checks that 
 * A is in the list. It then recurs on the tail with A.
 */
connected(A, L):-
    L == [] -> true;
    L = [H | T],
        node(A) -> findall(X,edge(X, H), Edges0), 
        findall(X, edge(H, X), Edges1),
        append(Edges0, Edges1, Edges),
        member(A, Edges) -> connected(A, T);
    false.


/** Question 5.2 */
maxclique(N, Cliques):-
    N < 2 -> Cliques = [];
    findall(X,node(X),Nodes), findallcliques(Nodes, Cliques).


findallcliques(N, C):-
    findcliques(N, C).

findcliques(Nodes, C):-
    Nodes == [] -> C = [];
    allConnected(Nodes) ->  Nodes = [H|T], findcliques(T, Temp), append(Temp, Nodes, C);
    Nodes = [H|T], findcliques(T, C).