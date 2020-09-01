% 0. From last time append
% http://www.swi-prolog.org/pldoc/man?section=debugger тук можете да видите опциите на Prolog за трасиране и дебъгване на програми
islist([]).
islist([_|_]).

append1([], L2, L2).
append1([H|T], L2, [H|R]):- append1(T, L2, R).

% 1. member 2x ways to say it with recursion or with append
member1(X, [X|_]).
member1(X, [_|T]):- member1(X, T).

member2(X, L):- append1(_, [X|_], L).

% 2. length of list with recursion and is_list

length1([], 0).
length1([_|T], N):- length1(T, M), N is M + 1.

% 3. take first element of list and last element of list
first(H, [H|_]).
last(X, L):- append1(_, [X], L).

% 4. reverse list
reverse1(L, RL):- rev(L, [], RL).
rev([], Stack, Stack).
rev([H|T], Stack, R):- rev(T, [H|Stack], R).
% Тук няма как без да се върнем обратно нагоре по рек. извиквания, за което се извинявам в час, че ви заблудих.

% *is_palindrome(L) :- L is a palindrome list

is_palindrome(L) :- reverse1(L,L).

% 5. insert element in list randomly in it
insert(X, L, RL):- append1(A, B, L), append1(A, [X|B], RL).
insert1(X, L, RL):- remove1(X, RL, L). % има опасност да зацикли след края на списъка

% 6. remove an element from a list
remove(X, L, RL):- append1(A, [X|B], L), append1(A, B , RL).
remove1(X, L, RL):- insert1(X, RL, L). % има опасност да зацикли след края на списъка

% 7. permutation of a list

permutation([], []).
permutation([H|T], P):- permutation(T, Q), insert(H, Q, P).

% 8. is_sorted and simplest sort
% Казахме и за логически еквивалентни формули. Има един Word документ на име QuantifierProblems погледнете го там ги има и ги запомнете
is_sorted(L):- not((append(_, [A,B|_], L), A > B)).
simplestSort(L, SL):- permutation(L, SL), is_sorted(SL).

/*
HW:
1. Напишете предикат пресмятащ сумата от елементите на списъка. прим [1,2,3,4] -> сумата е 10
2. Напишете предикат намиращ най-големия/ най-малкия елемент в списък. - има поне два начина да се каже. Единият е с рекурсия и помощна ф-я,
   а другия игра с квантори.
3. Напишете предикат, който е истина т.с.т.к. X дели Y. прим 5 дели 10, но 5 не дели 27.
*/


% 1.Sum
sum([], 0).
sum([H|T], N):- sum(T, M), N is H + M.


% 2.Min\Max element
% First way
% \+ означава пак not, но е по-лесно за изписване
min1(L, X):- member(X, L), \+ ((member(Y, L), X \= Y, Y < X)).
max1(L, X):- member(X, L), \+ ((member(Y, L), X \= Y, Y > X)).

% Second way
lesser(A, B, A):- A < B.

min2(X, [X]).
min2(X, [H|T]):- min2(M, T), lesser(H, M, X).

max2(X, [X]).
max2(X, [H|T]):- max2(M, T), \+ lesser(H, M, X).


% 3.Div
% Трябва да го има това оценяване стойността на Y, иначе се натрупват (...((Y - X) - X)... - X) и не се изчислява.
div(_, Y):- Y =:= 0.
div(X, Y):- Y >= X, div(X, Y - X).


% Може и така.
% Като има оператор за присвояване към нова променлива се форсира изчисляването на аритметичния израз от лявата страна
% преди да бъде присвоена неговата стойност на новата променлива
div1(_, 0).
div1(X, Y):- Y >= X, Y1 is Y - X, div1(X, Y1).
