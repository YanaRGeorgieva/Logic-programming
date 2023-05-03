/* 1. Казваме, че списък от списъци L е ламинарен, ако
всеки два списъка от L или нямат общи елементи, или всички
елементи на единия са елементи на другия.
Да се дефинира на пролог предикат isLaminar(L), който по да-
ден списък от списъци L проверява дали L е ламинарен. */

commonElement(X, Y, A):- member(A, X), member(A, Y).

condition1(X, Y):- not(commonElement(X, Y, _)).

isSubsetOf(X, Y):- not((member(A, X), not(member(A, Y)))).

condition2(X, Y):- isSubsetOf(X, Y); isSubsetOf(Y, X).

condition(X, Y):- condition1(X, Y); condition2(X, Y). % [] удовлетворява и двете.

isLaminar(L):- not(( member(X, L), member(Y, L), not(condition(X, Y)))).

/* Зад. 2. През 2020 год. поради корона вируса и инфлациите (фи-
нансова и морална) Дядо Коледа бил пред фалит и обявил на

всички жители на Необикновената страна, че годината ще е без
Коледа. Елфите даже декларирали, че от цветния картон им е
останало само едно парче с размери n × m (дължина×ширина).

На Баба Яга `и домъчняло (все пак и тя някога е била малка ве-
щица, очакваща да намери под елхата най-новата състезателна

метла) и решила да помогне поне до някъде да се спаси Коле-
да като подарят по една омагьосана картичка от цветен картон

за всяко дете. Така, преди да натовари картичките в дисаги-
те и да яхне метлата по света, тя се оказала със задачата да

определи какви да са размерите на k правоъгълни картички с
целочислени дължина и ширина, така че да `и стигне наличният
цветен картон за всичките картички, иначе трябва да ходи да
моли спонсори за поредните помощи. Задачата не е трудна: за

една картичка е необходимо да се изреже едно цяло парче цве-
тен картон, като разрезите трябва да са само хоризонтални и

вертикални. Баба Яга знаела, че не е много добра в сметките,
а въпросът бил спешен — конкуренцията с Дядо Мраз е много
остра и той разчита на същите спонсори. Помогнали `и студенти
от ФМИ.

Бъдете така добри да подарите на Баба Яга дефиниция на про-
лог на предикат saveChristmas(N, M, K, L), който по дадени

размери N и M на парчето цветен картон и брой K на кар-
тичките генерира в L всевъзможните решения — списъци от K

наредени двойки, дължина и ширина на всяка от K картички,
които могат да се изрежат от парчето цветен картон. */

% Ще решим задачата по два начина.

% Начин 1:
% N - височина, M - ширина
saveChristmas1(N, M, K, []):- N = 0; M = 0; K = 0.
saveChristmas1(N, M, K, L):- N > 0, M > 0, K > 0,
        generateCandidate(N, M, K, L, Coords), conditions(N, M, L, Coords).

generateKnumbersInRange(_, 0, _, []).
generateKnumbersInRange(Start, K, Limit, [El|R]):- K > 0, K1 is K - 1, 
        between(Start, Limit, El), generateKnumbersInRange(Start, K1, Limit, R).

max(A, B, A):- A >= B.
max(A, B, B):- A < B.

generateCandidate(N, M, K, L, Coords):- K2 is K * 2, 
        max(N, M, S), 
        generateKnumbersInRange(1, K2, S, L1), 
        generateKnumbersInRange(0, K2, S, Coords1), 
        makeOrderedPairs(L1, L), makeOrderedPairs(Coords1, Coords).

makeOrderedPairs([], []).
makeOrderedPairs([A, B|T], [[A, B]|R]):- makeOrderedPairs(T, R).

conditions(N, M, L, Coords):- 
        pack(L, Coords, Cards), allInRectangle(N, M, Cards), 
        noOverlappingRectangles(Cards).

pack([], [], []).
pack([[H, W]|T], [[X, Y]|T1], [[H, W, X, Y]|R]):- pack(T, T1, R).


/* Имаме, че правоъгълник се характеризира с точката (X, Y) и 
    ширина W и височина H.

(0,0)
|
+--------------------> Y axis
|
|    (X,Y)      (X, Y+W)
|    +--------------+
|    |              |
|    |              |
|    |              |
|    +--------------+
v    (X+H, Y)     (X+H,Y+W)

X axis */

allInRectangle(N, M, Cards):- 
        not((member([H, W, X, Y], Cards), 
                not(subRectangle([H, W, X, Y], [N, M, 0, 0])))).

noOverlappingRectangles(Cards):- 
        not((member(C1, Cards), member(C2, Cards), C1 \= C2, 
                not(rectanglesDoNotOverlap(C1, C2)))).

rectanglesDoNotOverlap([H, W, X, Y], [H1, W1, X1, Y1]):- 
        X1 + H1 < X ; X + H < X1 ; Y1 + W1 < Y ; Y + W < Y1.

subRectangle([H, W, X, Y], [H1, W1, X1, Y1]):- 
        X1 =< X, Y1 =< Y, Y + W =< Y1 + W1, X + H =< X1 + H1.

% Начин 2:
% Генерираме матрица NxM пълна с 0-ли и започваме да я пълним. 
% Ще индексираме всичко от 1.

% nthElem(?N, ?List, ?Elem, ?[Elem|Rest]).
nthElem(1, [X|Rest], X, [X|Rest]).
nthElem(N, [_|T], X, Rest):- nthElem(M, T, X, Rest), N is M + 1.


saveChristmas2(N, M, K, []):- N = 0; M = 0; K = 0.
saveChristmas2(N, M, K, L):- N > 0, M > 0, K > 0,
        N1 is N + 1, M1 is M + 1, % Ще ни трябват n+1 точки, за да кажем дължина n
        generateBoard(N1, M1, Board), 
        fillBoard(N1, M1, Board, K, L, NewBoard), 
        printBoard(NewBoard).

printBoard([]).
printBoard([H|T]):- printBoard(T), write(H), nl.

generateBoard(0, _, []).
generateBoard(N, M, [H|R]):- N > 0, N1 is N - 1, 
        repeat(0, M, H), generateBoard(N1, M, R).

repeat(_, 0, []).
repeat(X, N, [X|R]):- N > 0, N1 is N - 1, repeat(X, N1, R).

fillBoard(_, _, Board, 0, [], Board).
fillBoard(N, M, Board, K, [[H, W]|R], ResultBoard):- K > 0, K1 is K - 1,
    nthElem(X, Board, Row, XPlusAfterX), HeightLeftToFill is N - X, HeightLeftToFill >= 1,
    nthElem(Y, Row, 0, _), WidthLeftToFill is M - Y, 
    WidthLeftToFill >= 1, % Намерихме свободна клетка някъде в матрицата
    between(1, HeightLeftToFill, H), between(1, WidthLeftToFill, W),
    H1 is H + 1, W1 is W + 1, % Ще ни трябват n+1 точки, за да кажем дължина n
    fill(XPlusAfterX, Y, K, H1, W1, NewXPlusAfterX),
    append(BeforeX, XPlusAfterX, Board),
    append(BeforeX, NewXPlusAfterX, NewBoard),
    fillBoard(N, M, NewBoard, K1, R, ResultBoard).
    
fill(LeftRows, _, _, 0, _, LeftRows).
fill([Row|T], Y, Colour, H1, W1, [NewRow|R]):- H1 > 0, H11 is H1 - 1, 
        nthElem(Y, Row, 0, YPlusAfterY), 
        replaceElWithReplInList(YPlusAfterY, W1, 0, Colour, NewYPlusAfterY), 
        append(BeforeY, YPlusAfterY, Row),
        append(BeforeY, NewYPlusAfterY, NewRow),
        fill(T, Y, Colour, H11, W1, R).

replaceElWithReplInList(Left, 0, _, _, Left).
replaceElWithReplInList([El|T], W1, El, Repl, [Repl|R]):- 
        W1 > 0, W11 is W1 - 1, 
        replaceElWithReplInList(T, W11, El, Repl, R).