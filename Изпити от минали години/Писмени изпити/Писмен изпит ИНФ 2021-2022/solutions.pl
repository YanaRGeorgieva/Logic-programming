% Problem 1

ce_Pairs(N, C):- count_Pairs(N, C).

count_Pairs(N, C):-  count_Pairs(1, N, C).
count_Pairs(N, N, 0).
count_Pairs(A, N, C):- 
    A < N,
    condition(A, N),
    AP1 is A + 1, 
    count_Pairs(AP1, N, CAP1),
    C is CAP1 + 1.
count_Pairs(A, N, C):- 
    A < N,
    not(condition(A, N)),
    AP1 is A + 1, 
    count_Pairs(AP1, N, C).

condition(A, N):- 
    N mod A =:= 0, 
    B is N div A, 
    A =< B, 
    count_Divs(B, M), 
    MP1 is M + 1, 
    count_Divs(A, MP1).

count_Divs(N, C):- count_Divs(N, 1, C).

count_Divs(N, N, 1).
count_Divs(N, M, D):- 
    M < N, 
    N mod M =:= 0, 
    MP1 is M + 1, 
    count_Divs(N, MP1, DMP1), 
    D is DMP1 + 1.
count_Divs(N, M, D):- 
    M < N, 
    N mod M =\= 0, 
    MP1 is M + 1, 
    count_Divs(N, MP1, D).

% Problem 2
count_Neighbours([], _, _, 0).
count_Neighbours([[U, V]|T], U, Vis, N):- 
    not(member(V, Vis)), 
    count_Neighbours(T, U, [V|Vis], M),
    N is M + 1.
count_Neighbours([[V, U]|T], U, Vis, N):- 
    not(member(V, Vis)), 
    count_Neighbours(T, U, [V|Vis], M),
    N is M + 1.
count_Neighbours([[U, V]|T], U, Vis, N):- 
    member(V, Vis), 
    count_Neighbours(T, U, Vis, N).
count_Neighbours([[V, U]|T], U, Vis, N):- 
    member(V, Vis), 
    count_Neighbours(T, U, Vis, N).
count_Neighbours([[V1, V2]|T], U, Vis, N):-
    V1 \= U,
    V2 \= U,
    count_Neighbours(T, U, Vis, N).

node(E, U):- member(H, E), member(U, H).

ps_Gr(Edges, K):- not(( node(Edges, U), count_Neighbours(Edges, U, [], M), M < K )).
