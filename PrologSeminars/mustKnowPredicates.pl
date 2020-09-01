% Maybe you will find other valuable predicates, but I think that these are the top "must know".
% Definition of list
islist([]).
islist([_|_]).

% append(L1, L2, L3) where L3 = L1 + L2
append([], L2, L2).
append([H|T], L2, [H|R]):- append(T, L2, R).

% Is X a member of the list L
member(X, [X|_]).
member(X, [H|T]):- X \= H, member(X, T).

member1(X, L):- append(_, [X|_], L).

% Insert X in the list L arbitrary
insert(X, L, RL):- append(A, B, L), append(A, [X|B], RL).

% Remove X in the list L (Only from one position, if X is contained more than once in L)
remove(X, L, RL):- append1(A, [X|B], L), append1(A, B, RL).

% Permute the list L
permutation([], []).
permutation([H|T], P):- permutation(T, Q), insert(H, Q, P).

% Check if sorted
is_sorted(L):- not((append(_, [A,B|_], L), A > B)).
% The slowest sort algorithm, but valid
simplestSort(L, SL):- permutation(L, SL), is_sorted(SL).

% Length of list
length([], 0).
length([_|T], N):- length(T, M), N is M + 1.

% Minimal/Maximal element of list
min1(L, X):- member(X, L), \+ ((member(Y, L), X \= Y, Y < X)).
max1(L, X):- member(X, L), \+ ((member(Y, L), X \= Y, Y > X)).


% Prefix and suffix of list
prefix(P, L):- append(P, _, L).
suffix(S, L):- append(_, S, L).

% Sublist of list(a slice/chunk of the list)
sublist(R, L):- append(_, S, L), append(R, _, S).

% Subset of L
subset([], []).
subset([_|T], R):- subset(T, R).
subset([H|T], [H|R]):- subset(T, R).

% Get element at position N
n_th_element(X, 1, [X|_]).
n_th_element(X, N, [_|T]):- n_th_element(X, M, T), N is M + 1.

% Set predicates
in_union(X, A, B):- member(X, A); member(X, B).
in_intersection(X, A, B):- member(X, A), member(X, B).
in_difference(X, A, B):- member(X, A), \+ member(X, B).
is_subset_of(A, B):- \+((member(X, A), \+(member(X, B)))).
are_equal(A, B):- is_subset_of(A, B), is_subset_of(B, A).

% Reverse of lists
reverse(L, RL):- rev(L, [], RL).
rev([], Stack, Stack).
rev([H|T], Stack, R):- rev(T, [H|Stack], R).

% Erase duplicates in list L
removeDuplicates([], []).
removeDuplicates([H|T], [H|R]):- removeDuplicates(T, R), not( member(H, R)).
removeDuplicates([H|T], R):- removeDuplicates(T, R), member(H, R).


% Given the set of ribs E, get V the set of vertices
addV(V, VL, VR):- not(member(V, VL)), append([V], VL, VR).
addV(V, VL, VL):- member(V, VL).
vertices([], []).
vertices([[X, Y]|T], V):- vertices(T, TV), addV(X, TV, TX), addV(Y, TX, V).

% Find an acyclic path in the graph
path(E, X, Y, P):- path(E, X, Y, [], P).
path(_, _, Y, Vis, P) :- append([Y], _, Vis), reverse(Vis, P).
path(E, X, Y, Vis, P):- member([X, Z], E),  not(member(Z, Vis)),
		path(E, Z, Y, [X|Vis], P).

% When is there a cycle in the graph?
cycle(E, C):- member([X, Y], E), X \= Y, path(E, Y, X, P1), C = [X|P1].

% When is the graph connected?
connected(V, E):- not(( member(X, V), member(Y, V), X \= Y, not(path(E, X, Y, _)) )).

% Generate numbers in interval [A, B].
between(A, B, A):- A =< B.
between(A, B, R):- A < B, A1 is A + 1, between(A1, B, R).

% Split L in two subsets
split2([], [], []).
split2([H|T], [H|L], R):- split2(T, L, R).
split2([H|T], L, [H|R]):- split2(T, L, R).

% Generate natural numbers
nat(0).
nat(N):- nat(M), N is M + 1.

% Generate integers
integer(0, 0).
integer(X, Y):- X > 0, (Y is X; Y is -X).

% Generate a list of numbers in interval [A, B]
range(A, A, [A]).
range(A, B, [A|R]):- A < B, A1 is A + 1, range(A1, B, R).

% Generate a pai of naturals
pairs(A, B):- nat(N), between(0, N, A), B is N - A.

% Generate K numbeers with sum S
genKS(1, S, [S]).
genKS(K, S, [XI|Result]):- K > 1, K1 is K - 1, 
	between(0, S, XI), S1 is S - XI, genKS(K1, S1, Result).

% Generate all finite lists of natural numbers
genAll([]).
genAll(L):- nat(N), between(1, N, K),
			 S is N - K, genKS(K, S, L).

% Divide list in smaller lists
divide([], []).
divide(L, [H|R]):- append(H, T, L), H \= [], divide(T, R).
/*
?- divide([1,2,3],L).
L = [[1], [2], [3]] ;
L = [[1], [2, 3]] ;
L = [[1, 2], [3]] ;
L = [[1, 2, 3]] ;
false.
*/

% Opposite of divide
flatten(X, [X]):- not(is_list(X)).
flatten([], []).
flatten([H|T], R):- flatten(H, FH), flatten(T, FT), 
	append(FH, FT, R).

% Count occurrences of Y in L
count([], _, 0).
count([H|T], H, N):- count(T, H, M), N is M + 1.
count([H|T], Y, N):- H \= Y, count(T, Y, N).

/* Generate all trees defined
	[1] is a tree
	if A is a tree and B is a list of trees then [A|B] is a tree
	*/

t(T):- nat(N), main(N, T).

helper(1, [1]).
helper(N, X):- N > 1, M is N - 1, main(M, X).

main(0, []).
main(N, [H|T]):- N > 0, between(1, N, R), M is N - R, helper(R, H), main(M, T).

% Find the GCD of two numbers
gcd(A, 0, A).
gcd(A, B, G):- B > 0, C is A mod B, gcd(B, C, G).
