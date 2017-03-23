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
        member(H, L2) -> xdiff(T, L2, L);
    L1 = [H | T],
        xdiff(T, L2, S), 
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
    findall(X, node(X), Nodes), 
        Nodes = [H | T],
        findallcliques([H], T, [], Allcliques),
        filterbysize(N, Allcliques, Subcliques),
        toremove(Allcliques, Subcliques, Toremove), print(Toremove),
        xdiff(Subcliques, Toremove, Cliques).

/* These two predicates recursively find all of the cliques on the 
 * graph defined by the edges and nodes present */
findallcliques(Start, End, Ci, Co):-
    Start = [H | T],
        member([H], Ci)-> Co = Ci;
    append(Start, End, Nodes),
        findcliques(Nodes, C0),
        append(Ci, C0, C1),
        Start = [H | T],
        End = [H0 | T0],
        append(T0, [H], End0),
        findallcliques([H0], End0, C1, C2),
        Co = C2.

findcliques(Nodes, C):-
    Nodes == [] -> C = [];
    member(Nodes, C) == true -> print("found already");
    allConnected(Nodes) -> Nodes = [H | T], 
        findcliques(T, Temp), 
        append(Temp, [Nodes], Temp0),
        C = Temp0;
    Nodes = [H|T], 
        findcliques(T, C).

/* This predicate filters the items in a list by length of the item.
 * It assumes that the items are themsleves lists. */
filterbysize(Size, Cliquesin, Cliquesout):-
    Cliquesin == [] -> Cliquesout = [];
    Cliquesin = [H | T],
        length(H, Len),
        Len == Size -> filterbysize(Size, T, Temp),
        append([H], Temp, Cliquesout);
    Cliquesin = [H | T],
        filterbysize(Size, T, Temp),
        append(Temp, [], Cliquesout).

/* This predicate takes in a set and a subset of cliques. It checks that all
 * of the cliques in the subset of cliques are not a subset of a larger clique. 
 * If they are, it removes them from the list, and returns the (possibly) reduced
 * subset of cliques */
toremove(AC, SC, Result):-
    SC == [] -> Result = [];
    toremove2(AC, SC, Temp),
        SC = [SH | ST], 
        AC = [AH | AT],
        toremove(AC, ST, Temp0),
        append(Temp, Temp0, Result).

toremove2(AC, SC, Result):-
    AC == [] -> Result = [];
    SC = [SH | ST], 
        AC = [AH | AT],
        subset(SH, AH) , SH == AH -> toremove2(AT, SC, Temp),
        append([], Temp, Result);
    SC = [SH | ST], 
        AC = [AH | AT],
        subset(SH, AH) -> print("REMOVE THIS:"), print(SH), toremove2(AT, SC, Temp),
        append([SH], Temp, Result);
    AC = [AH | AT], 
        SC = [SH | ST], 
        toremove2(AT, SC, Temp),
        append([], Temp, Result).
