/*
	HW
	1. Реализирайте алгоритъм stree(V, E, ST), генериращ в ST покриващото дърво на графа G(V, E)
	2. Септемврийска сесия 2013 г.
	Зад. 1. Нека G е неориентиран граф. Множеството от вър-
	ховете на G е представено със списък V от върховете, всяко
	ребро v е представено с двуелементен списък на краища-
	та му, а множеството от ребрата на G е представено със
	списък E от ребрата.
	Да се дефинира на Пролог предикат
	а) con(V, E), който разпознава дали представеният с V и
	E граф е свързан.
	б) crit(V, E, X), който по дадени V и E на свързан граф ге-
	нерира в X списък на всички върхове, чието отстраняване
	води до граф, който не е свързан. (3 + 3 точки)
	3. 12.04.2014 г.
	Зад. 1. a) Да се дефинира на Пролог предикат
	p(X,A,B), който по даден списък от двойки X =
	[[a1, b1],[a2 ,b2 ],...,[an , bn ]] проверява дали шахматният
	кон може да се придвижи с един ход от поле с коорди-
	нати [A,B] на поле, чиито координати не са елемент на
	списъка X.
	б) Да се дефинира предикат q(X), който проверява да-
	ли шахматният кон може да се придвижи от поле с
	координати [1,1] на поле [8,8], без да преминава през
	полета, чиито координати са елемент на списъка X =
	[[a1, b1],[a2 ,b2 ],...,[an , bn ]].
	Забележка. Шахматната дъска е с размер 8 × 8.
*/

% 1.
stree(V, E , ST):- V = [H|T], st(V, E, [H], T, ST).
remove(X, L , NL):- append(A, [X|B], L), append(A, B, NL).

/* На всяка стъпка множеството от върхове е разбито на две множества -
	посетени и непосетени. Ние всеки път искаме да добавяме по един нов непосетен връх в множеството посетени,
	като има ребро от някой връх от множеството на посетените към този, който сега искаме да добавим.
*/
st(_, _, _, [], []).
st(V, E, Visited, NotYetVisited, [[U, W]|Result]):-
	member([U, W], E), member(U, Visited), not( member(W, Visited)),
	remove(W, NotYetVisited, NewNotYetVisited),
	st(V, E, [W|Visited], NewNotYetVisited, Result).

% 2.
path(E, X, Y, P):- path1(E, X, [Y], P).

path1(_, X, [X|Rest], [X|Rest]).
path1(E, X, [Y|Rest], P):- member([Z, Y], E),  not(member(Z, [Y|Rest])),
		path1(E, X, [Z,Y|Rest], P).

con(V, E):- not((member(X, V), member(Y, V), not(path(E, X, Y, _)))).

% Помощна функция проверяваща дали текущия връх е критичен.
critical(V, E, X):- append(A, [X|B], V), append(A, B, NV),
					member(U, NV), member(W, NV),
					not( path(E, U, W, _)). % Ако няма път от U до W, които са произволни, то значи X  е критичен връх.

crity(_, _, [], []). % Дъно.
crity(V, E, [CurrentV|T], [CurrentV|Result]):- critical(V, E, CurrentV), crity(V, E, T, Result). % Ако е критичен, го добавяме и продължаваме да търсим.
crity(V, E, [CurrentV|T], Result):- not(critical(V, E, CurrentV)), crity(V, E, T, Result). % Ако не е, го пропускаме и пак продължаваме да търсим.

crit(V, E, X):- crit(V, E, V, X). % Трябва да проверим всички върхове от V  без да променяме V, затова добявяме още един аргумент към ф-ята- треално по този списък итерираме.

% 3.
p(Forbidden, [X, Y]):- validMove([X, Y], Forbidden, _).

validMove([X, Y], Forbidden, [NX, NY]):- member([A, B], [[-2, -1], [-2, 1], [-1, 2], [1, 2], [2, 1], [2, -1], [1, -2], [-1, -2]]),
					NX is X + A, NY is Y + B, NX > 0, NY > 0, NX < 9, NY, < 9, not(member([NX, NY], Forbidden)).

q(Forbidden):- knightPath([1,1], [8,8], Forbidden, _).

knightPath(_, _, _, []).
knightPath(CurrPos, EndPoint, Forbidden, [CurrPos|Result]):- CurrPos \= EndPoint, validMove(CurrPos, Forbidden, NextPos),
													knightPath(NextPos, EndPoint, [CurrPos|Forbidden], Result).
