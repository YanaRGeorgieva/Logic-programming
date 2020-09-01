% 1. splitGen генератор
split([], []).
split(L, [H|R]):- append(H, T, L), H \= [], split(T, R).

splitN(0, L, L).
splitN(K, L, R):- K > 0, K1 is K - 1, split(L, T), splitN(K1, T, R).

splitGen(L, R):- nat(N), splitN(N, L, R).
/*
	Още сме на случая K = 2 тогава split([1,2,3], [[1], [2], [3]]) ни връща.
	Нека a = [1], b = [2], c = [3], т.е. имаме списък от [a, b, c].
	Продължаваме с splitN(1, [a, b, c], R).
	За К = 1 тогава split([a,b,c], [[a], [b], [c]]) и продължаваме с splitN(0, [[a], [b], [c]], R).
	Стигаме до дъното К = 0 и връщаме този списък [[a], [b], [c]] -> [[[1]], [[2]], [[3]]].
	Нека разгледаме и случая, когато L = [[[1, 2], [3]]].
	За K = 2 split([1,2,3], [[1, 2], [3]]) и нека a = [1, 2], а b = [3].
	Продължаваме с splitN(1, [a, b], R).
	За K = 1 split([a, b], [[a, b]]) ( Коректно е на предна стъпка сме генерирали [[a], [b]])
	Викаме след това splitN(0, [[a, b]], R).
	Стигаме до дъното К = 0 и връщаме този списък [[a, b]] -> [[[1, 2], 3]].
	Т.е. много по-разбираемо е, ако на всяко ниво резултатът го заместваме с някаква буквичка, защото
	много скоби ни пречи на това да разберем какво всъщност прави този предикат.
	После като заместим всички получаваме търсения израз.
*/
% 2. flatten
flatten(X, [X]):- not(is_list(X)).
flatten([], []).
flatten([H|T], R):- flatten(H, FH), flatten(T, FT), 
	append(FH, FT, R).

is_list([]).
is_list([_|_]).
/*
?- splitGen([1,2,3], R), flatten(R, L).
R = L, L = [1, 2, 3] ;
R = [[1], [2], [3]],
L = [1, 2, 3] ;
R = [[1], [2, 3]],
L = [1, 2, 3] ;
R = [[1, 2], [3]],
L = [1, 2, 3] ;
R = [[1, 2, 3]],
L = [1, 2, 3] ;
....
*/

% 3. Да се напише предикат генериращ всички дървета.
% [] is a tree
% if A and B are trees, then [A|B] is a tree
t(T):- nat(N), worker(N, T).

worker(0, []).
worker(N, [T1|T2]):- N > 0, N1 is N - 1, between(0, N1, NT1),
		NT2 is N1 - NT1, worker(NT1, T1), worker(NT2, T2).

/*
?- t(L).
L = [] ;
L = [[]] ;
L = [[], []] ;
L = [[[]]] ;
L = [[], [], []] ;
L = [[], [[]]] ;
L = [[[]], []] ;
L = [[[], []]] ;
L = [[[[]]]] ;
L = [[], [], [], []] ;
L = [[], [], [[]]] ;
....

*/


% 4.
/*
07.07.2014 г
Зад. 1. Да се дефинира на Пролог предикат p(A), кой-
то при преудовлетворяване генерира в A всички крайни
строго монотонно растящи аритметични прогресии от ес-
тествени числа.

	Можем да подходим по два начина:

 Начин 1 директно конструираме решението
 Начин 2 генерираме всички списъци и взимаме сато тези, които ни вършат работа, т.е. са 
 аритметични прогресии.
*/
nat(0).
nat(N):- nat(M), N is M + 1.

genKS(1, S, [S]).
genKS(K, S, [XI|R]):- K > 1, K1 is K - 1, between(0, S, XI), S1 is S - XI, genKS(K1, S1, R).

genArithProg(_, 0, _, []).
genArithProg(Current, N, Diff, [Current|Result]):-
	N > 0, N1 is N - 1, Next is Current + Diff, 
	genArithProg(Next, N1, Diff, Result).

firstApproach([]).
firstApproach(L):- nat(N), genKS(3, N, [Start, NumOfElem, Diff]), 
	Diff > 0, NumOfElem > 0,
	genArithProg(Start, NumOfElem, Diff, L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cond([]).
cond([_]).
cond([A, B]):- A < B.
cond(L):- (L = [X, Y, _ |_]), X < Y, 
	not(( append(_, [A, B, C|_], L), (A + C) =\=  (B + B) )).


pairs(A, B):- nat(N), between(0, N, A), B is N - A.

genAll([]).
genAll(L):- pairs(K, S), K > 0, S > 0, genKS(K, S, L).
secondApproach(L):- genAll(L), cond(L).


% 5.
/*Зад. 2. Списък от три числа [X,Y,R] ще интерпрети-
раме като окръжност с център hX,Y i и радиус R. 
a) Да се дефинира генератор circles1(X,Y,R,Z,T,S), който по
дадена окръжност [X,Y,R] при преудовлетворяване ге-
нерира в Z, T и S окръжностите, които съдържат ок-
ръжността [X,Y,R].
b) Да се дефинира генератор circles2(X,Y,R,Z,T,S), който по
дадена окръжност [X,Y,R] при преудовлетворяване ге-
нерира в Z, T и S окръжностите, които се съдържат ок-
ръжността [X,Y,R].
*/

subcircle([X, Y, R], [Z, T, S]):- sqrt((X - Z)^2 + (Y - T)^2) =< S - R.

integer(0, 0).
integer(X, Y):- X > 0, (Y is X; Y is -X).

% a) безкраен генератор е       % Това не бях оправила в час, че искаме 3 елемента само да генерираме, а не K
circles1([X, Y, R], [Z, T, S]):- nat(N), genKS(3, N, [Z1, T1, S]), S > 0,
	integer(Z1, Z), integer(T1, T), subcircle([X, Y, R], [Z, T, S]).
% b) краен генератор 
% - рисуваме квадратче, около окръжността [X,Y,R]
% - генерираме някакви окръжности в квадратчето
% - проверяваме дали са вътре в [X,Y,R]
circles2([X, Y, R], [Z, T, S]):- XLeft is X - R, XRight is X + R,
	YLow is Y - R, YHigh is Y + R,
	between(XLeft, XRight, Z), between(YLow, YHigh, T),
	between(1, R, S),
	subcircle([Z, T, S], [X, Y, R]).


% 6. 24 януари 2018 изпита
/*
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HW
nat(0).
nat(N):- nat(M), N is M + 1.

/*  alpha list [A, [A, B]]
	[] is an alpha list
	is A and B are alpha lists, then [A, [A, B]] is an alpha list
	?- alpha(L).
	L = [] ;
	L = [[], [[], []]] ;
	L = [[], [[], [[], [[], []]]]] ;
	L = [[[], [[], []]], [[[], [[], []]], []]] ;
	L = [[], [[], [[], [[], [[]|...]]]]] ;
	...
	*/
alpha(T):- nat(N), worker2(N, T).

worker2(0, []).
worker2(N, [T1,[T1,T2]]):- N > 0, N1 is N - 1, between(0, N1, NT1),
		NT2 is N1 - NT1, worker2(NT1, T1), worker2(NT2, T2).

/*
	11.06.2012 г.
	Зад. 2. Нека L е списък, който има следния вид:
	[[x 1 ,y 1 ],[x 2 ,y 2 ],...,[x n ,y n ]].
	Ще казваме, че L представя бинарната релация R, ако
	R = {(x 1 ,y 1 ),(x 2 ,y 2 ),...,(x n ,y n )}.
	Да се дефинира на Пролог:
	а) едноместен предикат s, който по даден списък L, пред-
	ставящ бинарната релация R, разпознава дали R е симет-
	рична релация.
	б) едноместен предикат t, който по даден списък L, предс-
	тавящ бинарната релация R, разпознава дали R е транзи-
	тивна релация.
	в) триместен предикат c, който по дадени два списъка L 1
	и L 2 , представящи съответно бинарните релации R 1 и R 2 ,
	генерира в L 3 списък, представящ композицията R 3 на
	R 1 и R 2 .
	Напомняне: (x,z) belongs to R 3 тогава и само тогава, когато има
	двойки (x,y) и (y,z), такива че (x,y) belongs to R 1 и (y,z) belongs to R 2 
	*/
s(R):- not(( member([X,Y], R), not(member([Y,X], R)) )).
t(R):- not(( member([X,Y],R), member([Y,Z],R), not(member([X,Z],R)) )).

compose(_, [], []).
compose([X, Y], [[Y, Z]|T], [[X, Z]|R]):- compose([X, Y], T, R).
compose([X, Y], [[Z, _]|T], R):- Y \= Z, compose([X, Y], T, R).

c([], _, []).
c([R1H|R1T], R2, Res):- compose(R1H, R2, C1), c(R1T, R2, C2), append(C1, C2, Res). 
/*
	07.07.2014 г.
	Зад. 1. Да се дефинира на Пролог предикат p(L), кой-
	то при преудовлетворяване генерира в L всички крайни
	строго монотонно растящи геометрични прогресии от ес-
	тествени числа с цяло частно. 
	*/

genKS(1, S, [S]).
genKS(K, S, [XI|R]):- K > 1, K1 is K - 1, between(0, S, XI), S1 is S - XI, genKS(K1, S1, R).

genProgression(0, _, _, []).
genProgression(K, Current, Quotient, [Current|Progression]):-
	K > 0, K1 is K - 1, Next is Current * Quotient, genProgression(K1, Next, Quotient, Progression).

p(L):- nat(N), genKS(3, N, [K, Start, Quotient]), K > 0, Start > 0, Quotient > 0, 
	genProgression(K, Start, Quotient, L).	 


/*	16 юни 2015 г.
	Зад. 2. Списък от три числа [X,Y,A] ще
	интерпретираме като квадрат с долен десен
	ъгъл hX,Y i и страна A. Да се дефинира генера-
	тор squares(X,Y,A,Z,T,B), който по даден квад-
	рат [X,Y,A] при преудовлетворяване генерира в Z, T
	и B квадратите, които съдържат квадрата [X,Y,A].
	*/
subsquare([X, Y, A], [Z, T, B]):- Z =< X, T =< Y, X + A =< Z + B.

integer(0, 0).
integer(X, Y):- X > 0, (Y is X; Y is -X).

squares([X, Y, A], [Z, T, B]):- nat(N), genKS(3, N, [Z1, T1, B]), B > 0,
	integer(Z1, Z), integer(T1, T), subsquare([X, Y, A], [Z, T, B]).