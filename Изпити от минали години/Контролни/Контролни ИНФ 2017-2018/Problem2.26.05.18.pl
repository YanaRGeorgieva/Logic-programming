nat(0).
nat(N):- nat(M), N is M + 1.

% Calculate the distance between two lists.
distanceBetweenTwoLists([], [], 0).
distanceBetweenTwoLists([H1|T1], [H1|T2], N):- distanceBetweenTwoLists(T1, T2, N).
distanceBetweenTwoLists([H1|T1], [H2|T2], N):- H1 \= H2, distanceBetweenTwoLists(T1, T2, M), N is M + 1.

% Calculate the distance between a list and a list of lists.
calculateDistance([], _, 0).
calculateDistance([H|T], Y, N):- distanceBetweenTwoLists(H, Y, M), calculateDistance(T, Y, O), N is M + O.

genOneBooleanList(0, []).
genOneBooleanList(N, [H|R]):- N > 0, N1 is N - 1, member(H, [0, 1]), genOneBooleanList(N1, R).

genAllBooleanLists(0, _, []).
genAllBooleanLists(N, K, [H|R]):- N > 0, N1 is N - 1, genOneBooleanList(K, H), genAllBooleanLists(N1, K, R).


% Get local center.
center(X, S):- member(Y, X), calculateDistance(X, Y, S), 
	not((member(Z, X), calculateDistance(X, Z, S1), S1 < S)).

% Make sure it is the best for our list.
main1(L, N):- center(L, S), 
	not((genOneBooleanList(N, Z), calculateDistance(L, Z, S2), S2 < S, not(member(Z, L)))).


p1(L, N):- nat(M), genAllBooleanLists(M, N, L), main1(L, N).

% Get local periphery.
periphery(X, S):- member(Y, X), calculateDistance(X, Y, S), 
	not((member(Z, X), calculateDistance(X, Z, S1), S1 > S)).

% Make sure it is the best for our list.
main2(L, N):- periphery(L, S), 
	not((genOneBooleanList(N, Z), calculateDistance(L, Z, S2), S2 > S, not(member(Z, L)))).

p2(L, N):- nat(M), genAllBooleanLists(M, N, L), main2(L, N).