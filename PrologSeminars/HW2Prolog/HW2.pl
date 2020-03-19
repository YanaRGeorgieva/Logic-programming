/*HW:
1. merge sort
2. substract т.е. A\B = C, където C  е раликата на множеството А без B
3. program generatig all numbers Y = k*X where k = 0, 1, 2...
4. Exponentiation by squaring - бърз алгоритъм за степенуване https://en.wikipedia.org/wiki/Exponentiation_by_squaring
5. 	 6.IV.2013 г.
Задача 1. Да се дефинира на пролог предикат p(L), който
по даден списък от различни списъци L проверява дали
в L съществуват два различни елемента, които имат общ
в L съществуват два различни елемента, които имат общ
елемент, който не принадлежи на никой друг елемент на L.
6. 	16 април 2015 г.
Зад. 2. Да се дефинира предикат p(X,Y ), който по даден
списък X генерира в Y всички списъци, чиито елементи са
елементи на X и броят на срещанията на най-често среща-
ния елемент в Y е число, което не е елемент на X.

*/

% 1.Merge sort
% Ако искате да виждате целия резултатен списък натиснете w[write]
/*
?- merge([1,2,3,4],[1,2,2,4,5,6], X).
X = [1, 1, 2, 2, 2, 3, 4, 4, 5|...] [write]
X = [1, 1, 2, 2, 2, 3, 4, 4, 5, 6] ;
false.
*/

less(A, B):- A =< B.
merge([], L, L).
merge(L, [], L).
merge([A|L1], [B|L2], [A|Res]):- less(A, B), merge(L1, [B|L2], Res).
merge([A|L1], [B|L2], [B|Res]):- not(less(A, B)), merge([A|L1], L2, Res).

% 2.Substract

substract([], _, []).
substract([H|T], L2, [H|Res]):- substract(T, L2, Res), not member(H, L2).
substract([H|T], L2, Res):- substract(T, L2, Res), member(H, L2).


% 3.div_gen
/* 1)-правило 1
   2)-правило 2
                            div_gen(5, Z).
							 /1)       ; \ 2)
							Z = 0  	     div_gen(5, Z), Z = 5 + Z1
											/ 1)   ; \ 2)
										Z1 = 0		 div_gen(5, Z1), Z1 = 5 + Z2
															/ 1)   ; \ 2)
														Z2 = 0		.........

   Част от безкрайното дърво, което се получава. Безкраен генератор на кратни числа на 5 в конретния случай.

*/
div_gen(_, 0).
div_gen(X, Y):- div_gen(X, Z), Y is X + Z.

% Може и
div_gen1(_, Y):- Y is 0.
div_gen1(X, Y):- div_gen(X, Z), Y is X + Z.



% 4.Exponentiation by squaring

% linear algo
pow(_, 1, 0).
pow(X, Y, N):- N > 0, N1 is N - 1, pow(X, Z, N1), Y is Z * X.

even(N):- N mod 2 =:= 0.
% logarithmic algo - просто съм разписала дефиницията
powFast(_, 1, 0).
powFast(X, Y, N):- N > 0, even(N), N1 is N / 2, powFast(X, Z, N1), Y is Z * Z.
powFast(X, Y, N):- N > 0, not even(N) , N1 is N - 1, powFast(X, Z, N1), Y is Z * X.

% 5.
% Използвам write, за да може да се видят свидетелите за true.
p(L):- member(A, L), member(B, L), A \= B, member(X, A), member(X, B), not(( member(C, L), C \= A, C \= B, member(X, C))).%, write(A), write(B), write(X).
% Можете и така да ги видите
p1(L, A, B, X):- member(A, L), member(B, L), A \= B, member(X, A), member(X, B), not(( member(C, L), C \= A, C \= B, member(X, C))).
/*
?- p1([[1,2,3],[1,3,4],[2,5,6,7], [1,2,3,4]], A, B, X).
A = [1, 3, 4],
B = [1, 2, 3, 4],
X = 4 ;
A = [1, 2, 3, 4],
B = [1, 3, 4],
X = 4 ;
false.
*/

% Избягваме повторенията по втория начин тип (i, j), (j, i).
p2(L):- append(_, [A|T], L), member(B, T), A \= B, member(X, A), member(X, B), not(( member(C, L), C \= A, C \= B, member(X, C))).%, write(A), write(B), write(X).

% 6
count(_, [], 0).
count(X, [X|T], N):- count(X, T, M), N is M + 1.
count(X, [H|T], N):- X \= H, count(X, T, N).
% без да искаме да са различни ще има проблем при преудовлетворяването, ще тръгне по второто правило

count_max(L, X, N):- member(X, L), count(X, L, N), not(( member(Y, L), count(Y, L, M), M > N)).

p(X, Y):- subset(X, X1), permutation(X1, Y), count_max(Y, _, N), not member(N, X).

/*
?- p([a,a,2,2,1,3,a,a,b,b], [a,a,a,a]).
true ;
true ;
true ;
true ;
false.

?- p([a,a,2,2,1,3,a,a,b,b], [a,a,a,3]).
false.
*/
