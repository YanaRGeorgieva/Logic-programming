/*
	24 януари 2018 изпита
	Зад. 1. Диаметър на списък наричаме разликата между броя срещания на най-често срещан
	елемент на списъка и броя срещания на най-рядко срещан елемент на списъка. Да се дефинира
	на Пролог едноместен предикат p, който по даден списък от списъци L разпознава дали:
	всички елементи на L имат един и същ диаметър.
	*/

count([], _, 0).
count([H|T], H, N):- count(T, H, M), N is M + 1.
count([H|T], Y, N):- H \= Y, count(T, Y, N).


mostFrequent([], 0).
mostFrequent(L, N):- member(X, L), count(L, X, N), 
	not((member(Y, L), count(L, Y, M), M > N)).


leastFrequent([], 0).
leastFrequent(L, N):- member(X, L), count(L, X, N), 
not((member(Y, L), count(L, Y, M), M < N)).

diameter(L, D):- mostFrequent(L, A), 
	leastFrequent(L, B), D is A - B.

p(L):- not((member(X, L), diameter(X, DX), member(Y, L), 
	diameter(Y, DY), DX \= DY)).


/* 
	[] is a tree 
    if A is a tree and B is a list of trees, then [A|B] is a tree
    
	Но, ако сменим условието на:
	[1] is a tree 
    if A is a tree and B is a list of trees, then [A|B] is a tree
    */
t(T):- nat(N), worker(N, T).

worker(0, [1]). %% wrong if we change the base from [] to [1]
worker(N, [T1|T2]):- N > 0, N1 is N - 1, between(0, N1, NT1),
		NT2 is N1 - NT1, worker(NT1, T1), worker(NT2, T2).


helper1(1, [1]).
helper1(N, X):- N > 1, M is N - 1, main1(M, X).

main1(0, []).
main1(N, [H|T]):- N > 0, between(1, N, R), M is N - R, helper1(R, H), main1(M, T).

/*
	24 януари 2018 изпита
	Фенски списък е краен списък, всеки елемент на който е някоя от буквите 1, 2 или е
	фенски списък, като никои два съседни елемента не са еднакви букви.
	Да се дефинира на Пролог едноместен предикат p(X), който при преудовлетворяване
	генерира в X всички фенски списъци, които се записват на Пролог с краен брой “[”
	*/

% Вариант от асистента
condy(X):- not(append(_,[1, 1|_],X)), not(append(_,[2, 2|_],X)).

helper(1, 1).
helper(2, 2).
helper(N, X):- N > 2, M is N - 3, main(M, X), condy(X).

main(0, []).
main(N, [H|T]):- N > 0, between(1, N, R), M is N - R, helper(R, H), main(M, T).

g(L, T):- helper(L, T).
g(L, T):- Y is L + 1, g(Y, T).

po(L):- g(3, L).

% Процедурен Вариант
% fenlist
nat(0).
nat(N):- nat(M), N is M + 1.

between(A, B, A):- A =< B.
between(A, B, C):- A < B, A1 is A + 1, between(A1, B, C).

pairs(A, B):- nat(N), between(1, N, A), B is N - A.

split([], []).
split(L, [H|R]):- append(H, T, L), H \= [], split(T, R).

splitN(1, L, L).
splitN(K, L, R):- K > 1, K1 is K - 1, split(L, T), splitN(K1, T, R).

gen([], 0).
gen([1|T], N):- N > 0, N1 is N - 1, gen(T, N1).
gen([2|T], N):- N > 0, N1 is N - 1, gen(T, N1).
gen([[]|T], N):- N > 0, N1 is N - 1, gen(T, N1).

generator([]).
generator(L):- pairs(A, B), A > 0, gen(L1, A), splitN(B, L1, L), condition(L).

condition(L):- not(is_list(L)). 
condition(L):- is_list(L), not(append(_,[1, 1|_], L)), not(append(_,[2, 2|_],L)), 
			not((member(X, L), not( condition(X)))).


/*  Задача 1 от Контролно на КН миналия семестър 18 ноември 2017
	вар 2
	*/

%% length([], 0).
%% length([_|T], N):- length(T, M), N is M + 1.

legim([]).
legim([H|T]):- length(H, 3), legim(T).

%% append([], B, B).
%% append([H|T], B, [H|R]):- append(T, B, R).

%% member(X, L):- append(_, [X|_], L).

% a)
% for all a from Y(for all b from Y([b, a, b] belongs to X ))
% not exists a from Y(exists b from Y([b, a, b] doesn't belong to X))
p1(X, Y):- not((member(A, Y), member(B, Y), not(member([B, A, B], X)) )).

% b)
q1(X, Y):- append(YL, YR, Y), p1(X, YL), p1(X, YR).

/*  Задача 2 от Контролно на КН миналия семестър 18 ноември 2017
	вар 2
	e-separator
	1) A union B = V
	2) A intersection B = empty and S is subset of E
	3) E diff S doesn't contain ribs connection a vertex 
	   from A with a vertex from B and vice-versa ->
	k-e-separator
	4) elem(A) >= k, elem(B) >= k, elem(S) =< k
	*/
% Дадени са ни V, E

%% Вариант от часа

separate(L, A, B):- perm(L, P), append(A, B, P).

separete1([], [], []).
separete1([H|T], [H|L], R):- separete1(T, L, R).
separete1([H|T], L, [H|R]):- separete1(T, L, R).


% a)
generate_partition(V, E, A, S, B):- generate_partition1(V, E, A, S, B, _).
generate_partition1(V, E, A, S, B, P):- separate(V, A, B),
	separate(E, P, S).

% b)
generate_eseparator(V, E, A, S, B):- generate_partition1(V, E, A, S, B, P), not((member([U,W], P), ((member(U, A), member(W, B)) ;  (member(W, A), member(U, B))) )).

% c)
length([], 0).
length([_|T], N):- length(T, M), N is M + 1.

generate_k_eseparator(V, E, K, A, S, B):- 
generate_eseparator(V, E, A, S, B), length(A, N), N >= K,
length(B, M), M >= K , length(S, P), P =< K.

% d)
generate_k_min_eseparator(V, E, K, A, S, B):-
generate_k_eseparator(V, E, K, A, S, B), length(S, N), 
not((generate_k_eseparator(V, E, K, _, S1, _) , 
	length(S1, M), M < N)).

%% Мой вариант
/*
split([], [], []).
split([H|T], [H|L], R):- split(T, L, R).
split([H|T], L, [H|R]):- split(T, L, R).

intersection(A, B):- member(X, A), member(X, B).

subset([], []).
subset([H|T], [H|R]):- subset(T, R).
subset([_|T], R):- subset(T, R).

% a)
generate_partition(V, E, A, S, B):- split(V, A, B),
	not(intersection(A, B)),
	subset(E, S). 

substract([], _, []).
substract([H|T], B, [H|R]):- not(member(H, B)), substract(T, B, R).
substract([H|T], B, R):- member(H, B), substract(T, B, R).

% b)
generate_eseparators(V, E, A, S, B):- generate_partition(V, E, A, S, B), substract(E, S, L), cond(A, B, L).
cond(A, B, L):- not(( member(V, A), member(U, B), (member([V, U], L); member([U, V], L)) )).

% c)
generate_k_eseparators(V, E, K, A, S, B):- generate_eseparators(V, E, A, S, B), length(A, LA), LA >= K, length(B, LB), LB >= K,
	length(S, LS), LS =< K.

% d)
generate_k_min_eseparators(V, E, K, A, S, B):- generate_k_eseparators(V, E, K, A, S, B), length(S, LS), 
	not(( generate_k_eseparators(V, E, K, _, S1, _), length(S1, LS1), LS1 < LS )).
*/
/*	19.11.2011 г.
	Задача 2. Да се дефинира на Пролог двуместен предикат,
	който по дадени две цели числа разпознава дали те имат
	едни и същи прости делители
	*/

% Цел 1: предикат генериращ всички прости делители на дадено число
decompose(1, _, []).
decompose(N, M, [M|R]):- N > 1, N mod M =:= 0, Next is N div M, decompose(Next, M, R).
decompose(N, M, R):- N > 1, N mod M =\= 0, NewM is M + 1, decompose(N, NewM, R).

removeDuplicates([], []).
removeDuplicates([H|T], [H|R]):- not(member(H, R)), removeDuplicates(T, R).
removeDuplicates([H|T], R):- member(H, R), removeDuplicates(T, R).

primeDivisors(N, L):- decompose(N, 2, L1), removeDuplicates(L1, L).

sort(L, S):- perm(L, S), not(( append(_, [A, B|_], S), A > B)).

p1(N, M):- primeDivisors(N, L1), sort(L1, P), primeDivisors(M, L2), sort(L2, P).

is_subset(A, B):- not((member(X, A), not(member(X, B)))).
are_equal(A, B):- is_subset(A, B), is_subset(B, A).

p2(N, M):- primeDivisors(N, L1), primeDivisors(M, L2), are_equal(L1, L2).

p3(N, M):- primeDivisors(N, L1), sort(L1, P), primeDivisors(M, L2), sort(L2, Q), (P = Q).