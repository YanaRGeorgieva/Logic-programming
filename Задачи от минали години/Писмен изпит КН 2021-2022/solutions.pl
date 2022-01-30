% Вариант 1 задача 1

edge(E, U, W):- member([U, W], E).

% Всички пътища от U го W с дължина K (не задължително прости a.k.a. ациклични).
pathLengthK(_, W, W, 0).
pathLengthK(E, U, W, K):- 
    K > 0, K1 is K - 1,
    edge(E, U, V),
    pathLengthK(E, V, W, K1).

% Имаме, че G е слабо свързан и K > 1.
pr_Gr(E, K):- 
    not(( edge(E, U, W), 
        not(pathLengthK(E, U, W, K)) )).
% Така получаваме, че за всяко U връх на графа: Adj(U) = Path_k(U), където Adj(U) е множеството на инцидентните върхове с U, а Path_k(U) е множеството от върхове достижими от U с K ребра по не непременно прости пътища. Като опростим условието за k-съвършен виждаме, че точно това се иска.

% Вариант 1 задача 2 с новата аритметика
:- use_module(library(clpfd)).
% Ползваме го да проверява дали сумата на L, списък от положителни числа, е N или да го генерира L по N.
sum([], N):- N #= 0.
sum([H|T], N):- N #> 0, H #> 0, M #= N - H, sum(T, M).

% Тъй като в sum ще се генерират всички разбивания на L (и пермутациите им), то няма смисъл да пермутираме L.
equPart(N, L):- sum(L, N), conditionVariant1(L), label(L).

conditionVariant1(L):- append(L1, L2, L), sum(L1, M), sum(L2, M).

% Вариант 1 задача 2 със аритметика

genKS(0, 0, []).
genKS(K, S, [XI|R]):-
    K > 0, K1 is K - 1, 
    between(1, S, XI), S1 is S - XI, 
    genKS(K1, S1, R).

rupture(N, L):- between(1, N, K), genKS(K, N, L).

equPartOld(N, L):- rupture(N, L), conditionVar1(L, N).

conditionVar1(L, N):- 
    N mod 2 =:= 0,
    N1 is N div 2,
    append(L1, L2, L), 
    length(L1, LL1), length(L2, LL2), 
    genKS(LL1, N1, L1), genKS(LL2, N1, L2).

