/*	03 септември 2016
	Зад. 1. Да се дефинира на пролог предикат p(L,N), който
	по подадени списък от естествени числа L и естествено чис-
	ло N проверява, че има N елемента a 1 ...a N на L такива,
	че за всеки N - 1 елемента b 1 ...b N-1 на L е изпълнено,
	че
	НОД(a 1 ,...,a N ) /= НОД(b 1 ,...,b N-1 ).
	*/

gcd(A, 0, A).
gcd(A, B, G):- B > 0, C is A mod B, gcd(B, C, G).

gcdAll([H], H).
gcdAll([H|T], G):- gcdAll(T, G1), gcd(H, G1, G).

p(L, N):- subset(L, A), length(A, N), gcdAll(A, G),  M is N - 1, not(( subset(L, B), length(B, M),
		gcdAll(B, G) )).


main([]):- assert(p2([1,2,3], 4)).

/*   Декартово произведение на два списъка.
	*/


helperCP([], _, []).
helperCP([H|T], B, [[H|[B]]|R]):- helperCP(T, B, R).

cartesianProduct(_, [], []).
cartesianProduct(A, [H|T], [New|R1]):- cartesianProduct(A, T, R1), helperCP(A, H, New).

%% Може и така с append.
%% helper([], _, []).
%% helper([H|T], B, R):- helper(T, B, R1), append([H], [B], New), append(New, R1, R).

%% cartesianProduct(_, [], []).
%% cartesianProduct(A, [H|T], R):- cartesianProduct(A, T, R1), helper(A, H, New), append(New, R1, R).


/*	Декартово произведение на списък от списъци.
	*/

cart([], []).
cart([H|T], [R|Res]):- member(R, H), cart(T, Res).

/*
	16 юни 2015
	Зад. 1. Да се дефинира предикат p(L,N), който по
	даден списък от положителни цели числа L и поло-
	жително цяло число N разпознава дали N може да се
	представи като произведение на няколко (не непремен-
	но различни) елемента на L.
	*/

prod([], 1).
prod([H|T], N):- prod(T, M), N is M * H.

p1(L, N):- subset(L, S), prod(S, N).


sum([], 0).
sum([H|T], N):- sum(T, M), N is M + H.

p2(L, N):- subset(L, S), sum(S, N).

/*
	03 септември 2016 
	Зад. 2. Казваме, че списък A от числа се поглъща от списък B
	от числа, ако сборът на всеки два елемента на A се съдържа
	в B. Да се дефинират на пролог предикати p(A,B), който по
	дадени списъци от числа A и B разпознава дали A се поглъща
	от B, и q(L,S), който по даден списък L от списъци от числа
	генерира в S максимална (по дължина) редица от различни еле-
	менти на L, за която е вярно, че всеки елемент на редицата се
	поглъща от всички елементи след него в S.

	for all X from A and Y from A ( X + Y is in B)
	*/
p3(A, B):- not(( member1(X, A), member1(Y, A), O is X + Y, not(member1(O , B)) )).

append1([], B, B).
append1([H|T], B, [H|R]):- append1(T, B, R).

member1(A,L):- append1(_, [A|_], L).

length1([], 0).
length1([_|T], N):- length1(T, M), N is M + 1.

psub([], []).
psub([_|T], R):- psub(T, R).
psub([H|T], R):- psub(T, R1), append1(A, B, R1), append1(A, [H|B], R).

removeDuplicates([], []).
removeDuplicates([H|T], [H|R]):- not(member1(H, R)), removeDuplicates(T, R).
removeDuplicates([H|T], R):- member1(H, R), removeDuplicates(T, R).


q1(L, S):- removeDuplicates(L, L1), psub(L1, S), 
	not(( append1(_, [A|T], S), member1(B, T), not(p3(A, B)) )). 
q(L, S):- q1(L, S), length1(S, N), not(( q1(L, S2), length1(S2, M), M > N)).


/*
	16 април 2016
	Зад. 1. Да се дефинира на пролог предикат p(L), който
	при преудовлетворяване генерира в L всички списъци от вида
	[X 1 ,X 2 ,...,X n ], където X i са списъци с елементи естествени
	числа между 0 и 99 и за всяко i, X i е префикс на X i+1
	*/

nat(0).
nat(N):- nat(M), N is M + 1.

pairs(A, B):- nat(N), between1(0, N, A), B is N - A.

generateLists(0, []).
generateLists(N, [H|T]):- N > 0, between1(0, 99, H), N1 is N - 1, generateLists(N1, T).

genKS(1, S, [S]).
genKS(K, S, [XI|R]):- K > 1, between1(1, S, XI), S1 is S - XI, K1 is K - 1, genKS(K1, S1, R). 

listsOfLists([], []).
listsOfLists([H|T], [New|R]):- generateLists(H, New), listsOfLists(T, R).

concat([], _, []).
concat([H|T], Buff, [Curr|R]):- append1(Buff, H, Curr), concat(T, Curr, R). 

p5(L):- pairs(K, S), K > 0, S > 0, genKS(K, S, L1), listsOfLists(L1, L2), concat(L2, [], L).


/*	
	15 юни 2016
	Зад. 1. Да се дефинира на пролог предикат p(L), който по да-
	ден списък L от списъци проверява дали за всеки два елемента
	на L, съществува трети елемент, съдържащ всички общи еле-
	менти на другите два.
	*/

p7(L):- not((member1(X, L), member1(Y, L), not((member1(Z, L), cond(X, Y, Z) )) )).
cond(X, Y, Z):- not(( member1(A, X), member1(A, Y), not( member1(A, Z)) )).

/*
	15 юни 2016	
	Зад. 2. Един списък от числа е базов, ако всеки два различни
	негови елементи са взаимно прости. Да се реализират на пролог
	следните предикати:
	a) base(L), който разпознава дали даден списък L е базов;
	б) gen(M,L), който по даден списък M от числа генерира в L
	всички базови списъци, чиито елементи са елементи на M;
	в) max(M,L), който по даден списък от числа M генерира в L
	всички базови списъци, чиито елементи са елементи и на M и
	които не се съдържат в по-големи базови списъци, чиито еле-
	менти са елементи и на M.
	*/
subset([], []).
subset([H|T], [H|R]):- subset(T, R).
subset([_|T], R):- subset(T, R).

gcd1(A, 0, A).
gcd1(A, B, G):- B > 0, C is A mod B, gcd1(B, C, G).

base(L):- not(( member1(X, L), member1(Y, L), X \= Y, not(gcd1(X, Y, 1)) )).

gen(M, L):- subset(M, L), base(L).

max(M, L):- gen(M, L), not(( gen(M, L1), L \= L1, is_subset(L, L1) )).

is_subset(A, B):- not((member1(X, A), not(member1(X, B)) )).

/*
	11.06.2012
	Зад. 1. Да се дефинира на Пролог двуместен предикат p,
	който по даден списък от списъци L генерира в M най-
	дългата обща подредица на елементите на L.
*/

first([H|_], H).

common([], _).
common([H|T], C):- sublisty(H, C), common(T, C).

sublisty(L, R):- append1(_, S, L), append1(R, _, S).

p7(L, M):- first(L, H), sublisty(H, M), common(L, M), length1(M, Len1), 
	not(( sublisty(H, M1), common(L, M1), length1(M1, Len2), Len2 > Len1)). 

/*  Намиране хроматично число на граф.
	зад 2 от 16 април 2016 е подобна
	Даден е граф G(V, E). Напишете на Prolog:
	а) предикат генериращ валидни оцветявания на графа.
	(валидно е, ако няма два свързани с ребво едноцветни върха)
	б) предикат намиращ минималния брой цветове нужни, за да оцветим графа.
	*/
% Помощен предикат генериращ в R числа в интервала [A, B].
between1(A, B, A):- A =< B.
between1(A, B, R):- A < B, A1 is A + 1, between1(A1, B, R).

generate_coloring([], _, []).
generate_coloring([H|T], C, [[H,I]|Result]):- between1(1, C, I), generate_coloring(T, C, Result).

valid_coloring(V, E, C, VC):- generate_coloring(V, C, VC), not(( member([A, B], E), 
															member([A, Col], VC), member([B, Col], VC) )).
/*
?- valid_coloring([a,b,c,d],[[a, b], [b, c], [c, d], [d, a]],1, C).false.

?- valid_coloring([a,b,c,d],[[a, b], [b, c], [c, d], [d, a]],2, C).
C = [[a, 1], [b, 2], [c, 1], [d, 2]] ;
C = [[a, 2], [b, 1], [c, 2], [d, 1]] ;
false.

?- valid_coloring([a,b,c,d],[[a, b], [b, c], [c, d], [d, a]],3, C).
C = [[a, 1], [b, 2], [c, 1], [d, 2]] ;
C = [[a, 1], [b, 2], [c, 1], [d, 3]] ;
C = [[a, 1], [b, 2], [c, 3], [d, 2]] ;
C = [[a, 1], [b, 3], [c, 1], [d, 2]] ;
C = [[a, 1], [b, 3], [c, 1], [d, 3]] ;
C = [[a, 1], [b, 3], [c, 2], [d, 3]] ;
C = [[a, 2], [b, 1], [c, 2], [d, 1]] ;
C = [[a, 2], [b, 1], [c, 2], [d, 3]] ;
...
false.
*/
% Хроматичното число няма да превишава броя на върховете в графа.
chromatic_number(V, E, C):-  length(V, N), between1(1, N, C), 
	valid_coloring(V, E, C, _), not(( C1 is C - 1, valid_coloring(V, E, C1, _) )).
/*
?- chromatic_number([a,b,c,d],[[a, b], [b, c], [c, d], [d, a]],C).
C = 2 ;Намиране хроматично число на граф.
C = 2 ;
false.

?- chromatic_number([a,b,c,d],[[a, b], [b, c], [c, d], [d, a], [b, a], [c, d], [d, c], [a, d], [a, c], [b, d]],C).
[[a,1],[b,2],[c,3],[d,4]]
C = 4 ;
...
false.
*/
