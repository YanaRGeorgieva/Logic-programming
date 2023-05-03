% Problem 1

commonElement([A, B], [C, D]):- B - C >= 0, D - A >= 0.

normal(L):- not(( member(X, L), member(Y, L), X \= Y, commonElement(X, Y) )).

quickSort([], []).
quickSort([H|T], R):- 
    partition(T, H, LL, RL), 
    quickSort(LL, SLL), 
    quickSort(RL, SRL), 
    append(SLL, [H|SRL], R).

partition([], _, [], []).
partition([H|T], Pivot, [H|LL], RL):- lessEq(H, Pivot), partition(T, Pivot, LL, RL).
partition([H|T], Pivot, LL, [H|RL]):- not(lessEq(H, Pivot)), partition(T, Pivot, LL, RL).

lessEq([A, _], [C, _]):- A =< C.

max(X, Y, X):- Y =< X.
max(X, Y, Y):- not(Y =< X).

mergeOverlappingIntervals([X], [X]).
mergeOverlappingIntervals([[A, B], [C, D]|T], R):-
    commonElement([A, B], [C, D]), 
    min(A, C, X), max(B, D, Y), 
    mergeOverlappingIntervals([[X, Y]|T], R).
mergeOverlappingIntervals([[A, B], [C, D]|T], [[A, B]|R]):-
    not(commonElement([A, B], [C, D])), 
    mergeOverlappingIntervals([[C, D]|T], R).

volume(L, X):- quickSort(L, SL), mergeOverlappingIntervals(SL, X).

normalize_c(L, L):- normal(L).
normalize_c(L, X):- not(normal(L)), volume(L, X).

/*
?- normalize_c([[1,4], [3,6], [2, 7], [10, 98]], X).
X = [[1, 7], [10, 98]] ;
false.
*/

% Problem 2

prime(P):- P > 1, N is round(sqrt(P)), not(( between(2, N, M), P mod M =:= 0 )).

min(X, Y, X):- X =< Y.
min(X, Y, Y):- not(X =< Y).

coPrime(A, B):- min(A, B, C), not(( between(1, C, N), A mod N =:= 0, B mod N =:= 0, N =\= 1 )).

acceptable(A, B):- A \= B, not(coPrime(A, B)).

stable(L):- not(( member(AI, L), member(AJ, L), acceptable(AI, AJ) )).

genPrimeDivisor(A, B, R, P):- between(2, R, P), prime(P), A mod P =:= 0, B mod P =:= 0.

lcpd(A, B, P):- min(A, B, C), genPrimeDivisor(A, B, C, P), 
    not((genPrimeDivisor(A, B, C, P1), P1 < P)).

transfer(L, L, 0):- stable(L).
transfer(L, R, N):- N > 0,
    append(X, [AI|Y], L), append(Z, [AJ|T], Y),
    acceptable(AI, AJ), 
    N1 is N - 1,
    lcpd(AI, AJ, P),
    ( ( AIM is AI * P, AJD is AJ div P, append(Z, [AJD|T], Y1), append(X, [AIM|Y1], L1), transfer(L1, R, N1) ); 
      ( AJM is AJ * P, AID is AI div P, append(Z, [AJM|T], Y1), append(X, [AID|Y1], L1), transfer(L1, R, N1) ) ).


getAverage(L, Avg):- sum(L, Sum), length(L, N), Avg is round(Sum // N).

sum([], 0).
sum([H|T], S):- sum(T, S1), S is S1 + H.

min_stable(L, 0):- stable(L).
min_stable(L, N_exchange):- 
    not(stable(L)),
    getAverage(L, Avg),
    between(1, Avg, N_exchange), transfer(L, X, N_exchange),
    not(( between(1, Avg, M_exchange), transfer(L, _, M_exchange), M_exchange < N_exchange )), write(X).