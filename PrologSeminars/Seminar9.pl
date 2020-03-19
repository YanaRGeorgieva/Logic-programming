% 1. definition of graph
/*	Ориентиран граф.
	G(V, E)
	- V - множество от върхове
	- E - наредено множество от двойки (i,j), където последното значи, че от връх i има ребро към връх j
	Във почти всички задачи, които ще решаваме с графи ще ви бъдат дадени  или само E, или и двете (най-благоприятния случай).
	Ще се научим и как от E да получим V, но първо как ще представяме Е, а V?
	E = [[a, b], [b, c], [c, e], [e, a]] - списък от списъци
	V = [a, b, c, e]
	Може и да е даден списък L = [[a, b, c], [b, a], [c, a, d]] списък на съседство.
*/

% 2. Как по дадено множество ребра E да възстановим кои са върховете V?
% Помощна функция преценяваща кога да добавим нов връх към Vcurrent.
addV(V, VL, VR):- not(member(V, VL)), append([V], VL, VR).
addV(V, VL, VL):- member(V, VL).
% Main ф-я
vertices([], []).
vertices([[X, Y]|T], V):- vertices(T, TV), addV(X, TV, TX), addV(Y, TX, V).
/*
?- vertices([[a, b], [b, c], [c, e], [e, a]], X).
X = [b, c, a, e] ;
false.
*/

% Друг начин:
% - Конкатенираме Е в нов списък L
% - Премахваме повторенията в L
megaConcat([], []).
megaConcat([H|T], R):- megaConcat(T, Q), append(H, Q, R).

/*
?- megaConcat([[a, b], [b, c], [c, e], [e, a]], L).
L = [a, b, b, c, c, e, e, a].
*/
toSet([], []).
toSet([H|T], [H|R]):- toSet(T, R), not( member(H, R)).
toSet([H|T], R):- toSet(T, R), member(H, R).

/*
?- to_set([a, b, b, c, c, e, e, a], S).
S = [b, c, e, a] ;
false.
*/
vertices2(E, V):- megaConcat(E, L), toSet(L, V).

% Какво е път в граф?
/* Път в ориентиран (неориентиран) граф G(V,A) се нарича последователност от върхове v1, v2, … vk, такива че за всяко i = 1, 2… k-1 е в сила (vi,vi+1)∈A.
Върховете v1 и vk се наричат краища на пътя.
Ако v1 = vk, то има цикъл. Ако няма повтаряне на върхове в даден път (в частност цикъл), тогава пътят е прост ( за всяко i≠j (1≤i,j≤k) следва vi≠vj).
Ако даден граф има път от всеки връх до всеки друг - графът е свързан.
С O(n) сложност, където n е броят на дъгите, може да се определи дали даден граф е свързан или не (DFS).
*/
% 3. Кога между два върха X и Y има път?
% Ние ще решим задачата конструикайки пътя от Y към X  с натрупване.
% В този случай третият ми аргумент служи както за масив, където слагам вече посетените върхове
% така и за място, където конструирам резултатния списък.
% Извикваща ф-я
path(E, X, Y, P):- path1(E, X, [Y], P).
% Main ф-я
path1(_, X, [X|Rest], [X|Rest]).
path1(E, X, [Y|Rest], P):- member([Z, Y], E),  not(member(Z, [Y|Rest])), path1(E, X, [Z,Y|Rest], P).

% Версия с 5 параметъра
path2(E, X, Y, P):- path3(E, X, Y, [], P).
% Main ф-я
path3(_, _, Y, Visited, P) :- append([Y], _, Visited), reverse(Visited, P).
path3(E, X, Y, Visited, P):- member([X, Z], E),  not(member(Z, Visited)),
		path3(E, Z, Y, [X|Visited], P).
/*
?- path([[a, b], [b, c], [c, e], [e, a], [g, a]], a, c, P).
P = [a, b, c] ;
false.
?- path([[a, b], [b, c], [c, e], [e, a], [g, a]], a, g, P).
false.
*/

% 4. Кога има цикъл в граф? Изключваме примките.
cycle(E, [X|P1]):- member([X, Y], E), X \= Y, path(E, Y, X, P1).
/*
?- cycle([[a, b], [b, c], [c, e], [e, a], [g, a]], C).
C = [a, b, c, e, a] ;
C = [b, c, e, a, b] ;
C = [c, e, a, b, c] ;
C = [e, a, b, c, e] ;
false.

?- cycle([[a, b], [b, b], [c, e], [e, a], [g, a]], C).
false.

?- cycle([[a, b], [b, a], [c, e], [e, a], [g, a]], C).
C = [a, b, a] ;
C = [b, a, b] ;
false.
*/

% 5. Кога един граф е свързан? Разписване дефиниция:
% Когато има път от всеки връх, към всеки друг, т.е т.с.т.к не съществуват два върха,
% които да не са свързани с път.                    Изключваме примките.
connected(V, E):- not(( member(X, V), member(Y, V), X \= Y, not(path(E, X, Y, _)) )).
/*
?- connected([a,b,c,e,g], [[a, b], [b, a], [c, e], [e, a], [g, a]]).
false.

?- connected([a,b,c,e,g], [[a, b], [b, c], [c, e], [e, a]]).
false. % g е изолиран връх

?- connected([a,b,c,e], [[a, b], [b, c], [c, e], [e, a]]).
true.
*/

% 6. DFS
% Помощна функция генерираща следващия връх
gen_next_vertice(E, Curr, Container, Visited, Next):-
	member([Curr, Next], E),
	not(member(Next, Container)),
	not(member(Next, Visited)).
% Извикваща ф-я
dfs(E, Root, Result):- dfs_main(E, [Root], [], Result).
% Main ф-я
dfs_main(_, [], _ , []).
dfs_main(E, [StackH|StackT], Visited, [[StackH, Next]|Result]):-
	gen_next_vertice(E, StackH, [StackH|StackT], Visited, Next),
	dfs_main(E, [Next, StackH|StackT], Visited, Result).
dfs_main(E, [StackH|StackT], Visited, Result):-
	not(gen_next_vertice(E, StackH, [StackH|StackT], Visited, _)),
	dfs_main(E, StackT, [StackH|Visited], Result).

/*
?- dfs( [[a, b], [b, a], [c, e], [e, a], [g, a]], a, P).
P = [[a, b]] ;
false.

?- dfs( [[a, b], [b, c], [c, e], [e, a], [g, a]], a, P).
P = [[a, b], [b, c], [c, e]] ;
false.

?- dfs( [[a, b], [b, c], [c, e], [e, a], [g, a]], g, P).
P = [[g, a], [a, b], [b, c], [c, e]] ;
false.

?- dfs( [[a, b], [a, c], [a, e], [b, d], [e, o]], a, P).
P = [[a, b], [b, d], [a, c], [a, e], [e, o]] ;
P = [[a, b], [b, d], [a, e], [e, o], [a, c]] ;
P = [[a, c], [a, b], [b, d], [a, e], [e, o]] ;
P = [[a, c], [a, e], [e, o], [a, b], [b, d]] ;
P = [[a, e], [e, o], [a, b], [b, d], [a, c]] ;
P = [[a, e], [e, o], [a, c], [a, b], [b, d]] ;
false.
*/

% А bfs? Какво трябва да сменим?
% Извикваща ф-я
bfs(E, Root, Result):- bfs_main(E, [Root], [], Result).
% Main ф-я
bfs_main(_, [], _ , []).
bfs_main(E, [QueueH|QueueT], Visited, [[QueueH, Next]|Result]):-
	gen_next_vertice(E, QueueH, [QueueH|QueueT], Visited, Next),
	append([QueueH|QueueT], [Next], NewQueue), % Тук е
	bfs_main(E, NewQueue, Visited, Result). % промяната
bfs_main(E, [QueueH|QueueT], Visited, Result):-
	not(gen_next_vertice(E, QueueH, [QueueH|QueueT], Visited, _)),
	dfs_main(E, QueueT, [QueueH|Visited], Result).
/*
?- bfs( [[a, b], [a, c], [a, e], [b, d], [e, o]], a, P).
P = [[a, b], [a, c], [a, e], [b, d], [e, o]] ;
P = [[a, b], [a, e], [a, c], [b, d], [e, o]] ;
P = [[a, c], [a, b], [a, e], [b, d], [e, o]] ;
P = [[a, c], [a, e], [a, b], [e, o], [b, d]] ;
P = [[a, e], [a, b], [a, c], [e, o], [b, d]] ;
P = [[a, e], [a, c], [a, b], [e, o], [b, d]] ;
false.
*/

% 7. Hamitonian cycle, https://en.wikipedia.org/wiki/Hamiltonian_path
/* Хамилтонов цикъл е цикъл, който включва всички възли на графа точно по веднъж.
  Напишете предикат на Prolog hamiltonian_path(V, E, C),
  който по дадени V и E генерира в трети аргумент
  C хамилтоновите цикли в графа.
  Каква е дефиницията на хамилтонов път?
  Път включващ точно веднъж всеки връх.
*/
permutation([], []).
permutation([H|T], P):- permutation(T, Q), append(A, B, Q), append(A, [H|B], P).

hamiltonian_path(V, E, P):- permutation(V, P), not((append(_, [A,B|_], P), not(member([A, B], E)) )).
/*
?- hamiltonian_path([a,b,c,d,e,o],[[a, b], [a, c], [a, e], [b, d], [e, o]], P).
false.

?- hamiltonian_path([a,b,c,d],[[a, b], [b, c], [c, d], [d, a]], P).
P = [a, b, c, d] ;
P = [b, c, d, a] ;
P = [c, d, a, b] ;
P = [d, a, b, c] ;
false.

?- hamiltonian_path([a,b,c,d],[[a, b], [b, c], [c, d]], P).
P = [a, b, c, d] ;
false.
*/
hamiltonian_cycle(V, E, C):- hamiltonian_path(V, E, [H|T]), append(_, [Y], T), member([Y, H], E), append([H|T], [H], C).
/*
?- hamiltonian_cycle([a,b,c,d],[[a, b], [b, c], [c, d], [d, a]], P).
P = [a, b, c, d, a] ;
P = [b, c, d, a, b] ;
P = [c, d, a, b, c] ;
P = [d, a, b, c, d] ;
false.

?- hamiltonian_cycle([a,b,c,d],[[a, b], [b, c], [c, d]], P).
false.

?- hamiltonian_cycle([a],[[a, b], [b, c], [c, d], [d,a]], P).
false.
*/

% 8.
/*
	21 ноември 2015 г.
	Зад. 2. Нека E е списък от двуелементни списъци като всеки
	двуелементен списък [u,v] интерпретираме като ребро от връх u
	към v в граф. Да се дефинират на Пролог следните предикати:
	∙ p(E), който разпознава дали зададеният с E граф е цик-
	личен. Един граф е цикличен, ако ребрата му образуват
	цикъл, в който през всяко ребро и през всеки връх се ми-
	нава точно по веднъж.
	∙ q(E), който разпознава дали зададеният с G граф е ос-
	морка. Един граф е осморка, ако е обединение на два цик-
	лични графа, които имат точно един общ връх помежду
	си.
	Заб.: Граф, който е осморка, може така да се разположи в рав-
	нината, че да изписва цифрата осем.

	∙ q(E), който разпознава дали зададеният с G граф е три-
	листна детелина. Един граф е трилистна детелина, ако
	е обединение на три циклични графа, които имат точно
	един общ връх и никои два от тях нямат друг общ връх.
	Заб.: Граф, който е трилистна детелина, може така да се разпо-
	ложи в равнината, че да изобразява трилистна детелина.


	За да разберем дали един граф е цикличен, то трябва да направим пермутация на върховете и да проверим:
	- дали започва и завършва в същия връх
	- да няма връх, от който да излиза повече от едно ребро (всъщност това осигурява да няма вътрешен цикъл)
	- и все пак да е коректен цикъла, т.е. [[..., [u, v], [v, w], ...]]
*/
p(E):- p1(E, _).
/*
?- p([[a,b],[b,c],[c,a]]).
true ;
true ;
true ;
false.

?- p([[a,b],[b,c],[c,a], [c, e]]).
false.
*/

% Ще ми трябва да знам началния връх за втората подточка.
p1(E, S):- permutation(E, P), P = [[S,_]|_], append(_, [[_, S]], P), %Съжалявам в час съм била написала P = [H|T], append(_, [H], P) в бързината.
% Щеше да е валидно, ако работихме със списък от върховете.
			not(( append(_, [[X, _]|T], P), member([X, _], T) )),
 			not(( append(_, [[_, U], [V, _]|T], P), U \= V )).
/*
?- p1([[a,b],[b,c],[c,a]], S).
S = a ;
S = b ;
S = c ;
false.
*/

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
