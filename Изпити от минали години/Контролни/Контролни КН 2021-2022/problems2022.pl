:- use_module(library(clpfd)). % Only integers - finite domains. 

% Utilities
natural(0).
natural(N):- natural(M), N is M + 1.

subsequence([], []).
subsequence([H|T], [H|R]):- subsequence(T, R).
subsequence([_|T], R):- subsequence(T, R).

intersection([], _, []).
intersection([H|T], B, [H|R]):- intersection(T, B, R), member(H, B).
intersection([H|T], B, R):- intersection(T, B, R), not(member(H, B)).

sumSecondCoordinate([], 0).
sumSecondCoordinate([[_,S]|T], N):- sumSecondCoordinate(T, M), N is S + M.

% Problem 1
partOfNivkaWithWeights(PartOfNivka, Nivka):- 
    subsequence(PartOfNivka, Nivka).

indepen(L):- partOfNivkaWithWeights(A, L), 
    partOfNivkaWithWeights(B, L), 
    intersection(A, B, C), 
    condition(A, B, C).

condition(A, B, C):- C /= [], 
    sumSecondCoordinate(A, SA), 
    sumSecondCoordinate(B, SB), 
    sumSecondCoordinate(C, SC), 
    SC =:= SA * SB.


% Problem 2
% Алтернативно редува 0 нули, 1 ед, 2 нули и тн, но мултиплицирани по 3 пъти (за да осигурни условието с остатъка, но може да се пропусне, и така си е осигурено)
aperiodic([M, Y]):- natural(N), 
    between(0, N, R), Y is N mod 2, between(0, 2, K), M is 3*((N*N + N)/2 + R) + K.

% Идеята е, че ако нещо е точен квадрат, d*d, d*d+d няма да е и ще има друга стойност, така осигуряваме условието.
aperiodic2([M, Y]):- natural(M), value(M, Y).
cond(M):- M mod 3 #= 1, N #= M // 3, D in 0..N, N #=D*D.
value(M, 1):- cond(M). 
value(M, 0):- not(cond(M)).

% Следва двоичния запис на sqrt(2). На всяка стъпка намира поредния бит.
bsqrt(A, A, 0, 0, 0).
bsqrt(A, X1, K, B, N1):- bsqrt(A, X, Y, _, N), N1 is N + 1, 
    next(X, Y, N1, K, B), X1 is X * 4.

next(X, Y, N, K, 1):- K is 2*Y + 1, K*K < X, !.
next(X, Y, N1, K, 0):- K is 2*Y.
aperiodic3([N, B]):- bsqrt(2, _,_,B, N).





