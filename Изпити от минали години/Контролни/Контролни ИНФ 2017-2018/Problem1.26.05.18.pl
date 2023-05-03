min(L, X):- member(X, L), \+ ((member(Y, L), X \= Y, Y < X)).
max(L, X):- member(X, L), \+ ((member(Y, L), X \= Y, Y > X)).

getMax([], []).
getMax([H|T], [C|R]):- max(H, C), getMax(T, R).

getMin([], []).
getMin([H|T], [C|R]):- min(H, C), getMin(T, R).

balance([], 0).
balance(L, N):- getMax(L, Max), getMin(L, Min), max(Min, M1), min(Max, M2), N is M2 - M1. 

p1(L):- balance(L, B), not(( member(Y, L), not(member(B, Y)))). 

length([], 0).
length([_|T], N):- length(T, M), N is M + 1.

p2(L):- balance(L, B), length(L, N), B > N.


