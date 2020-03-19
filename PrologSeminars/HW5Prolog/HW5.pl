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