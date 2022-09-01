% Problem 1

sumPrices([], 0).
sumPrices([[_, C, _]|T], N):- sumPrices(T, M), N is M + C.

generateMenu(X, [[предястие, N1, D1], [основно, N2, D2], [предястие, N3, D3]]):- 
    remove([предястие, N1, D1], X, _), 
    remove([предястие, N3, D3], X, RestX), 
    remove([основно, N2, D2], RestX, _). 

p(X, N, M):- generateMenu(X, M), sumPrices(M, Cost), Cost =< N.

% Problem 2

partition([], [], []).
partition([H|T], [H|R], L):- partition(T, R, L).
partition([H|T], R, [H|L]):- partition(T, R, L).

sumWeights([], 0).
sumWeights([[W, _]|T], N):- sumWeights(T, M), N is M + W.

remove(X, L, R):- append(A, [X|B], L), append(A, B, R).

selectRower(Rower, List, Rest):- remove(Rower, List, Rest).

reach(L, W, N):- getThemToTheRightSide(L, [], W, N).

getThemToTheRightSide([], _, _, N):- N >= 0.
getThemToTheRightSide(OnLeftSide, OnRightSide, W, N):- 
    OnLeftSide \= [], N > 0, N1 is N - 1,
    partition(OnLeftSide, OnBoat, LeftOnLeftSide), 
    sumWeights(OnBoat, OnBoatW), OnBoatW =< W, 
    selectRower([RowerW, RowerC], OnBoat, OnBoatNoRower), 
    RowerC > 0, NewRowerC is RowerC - 1, 
    append(OnRightSide, [[RowerW, NewRowerC]|OnBoatNoRower], NewOnRightSide),
    getThemToTheLeftSide(LeftOnLeftSide, NewOnRightSide, W, N1).

getThemToTheLeftSide([], _, _, N):- N >= 0.
getThemToTheLeftSide(OnLeftSide, OnRightSide, W, N):- 
    OnLeftSide \= [], N > 0, N1 is N - 1,
    partition(OnRightSide, OnBoat, LeftOnRightSide), 
    sumWeights(OnBoat, OnBoatW), OnBoatW =< W, 
    selectRower([RowerW, RowerC], OnBoat, OnBoatNoRower), 
    RowerC > 0, NewRowerC is RowerC - 1, 
    append(OnLeftSide, [[RowerW, NewRowerC]|OnBoatNoRower], NewOnLeftSide),
    getThemToTheRightSide(NewOnLeftSide, LeftOnRightSide, W, N1).