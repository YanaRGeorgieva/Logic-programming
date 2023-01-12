/* Задача 1
Най-важното събитие в живота на древния Йосиф, той и неговите войници са хванати от римляните. 
Те решили да извършат масово самоубийство като оформят кръг и убиват всеки K-ти докато остане един, който след това ще се самоубие. 
Редът се нарича пермутация на Йосиф.
Напишете програма на пролог josephus(M, K, P), която по списък от хората М и интервал K, генерира в P пермутацията P.
*/
% Индексираме списъците от 0.
josephus(M, K, P):- josephus(M, K, 0, P). % Start at index 0.
josephus([X], _, _, [X]).
josephus(Circle, K, Start, [X|R]):- 
    length(Circle, N),
    N > 1,
    Start1 is (Start + K) mod N,
    nthRest(X, Start1, Circle, RestCircle),
    josephus(RestCircle, K, Start1, R).

nthRest(X, 0, [X|R], R).
nthRest(X, N, [Y|T], [Y|R]):-
    nthRest(X, M, T, R),
    N is M + 1.

% Вар 2 в Иванчо, за Данчо е подобно.
ivan_last_erased([K], K).
ivan_last_erased([H|T], K):- 
    T \= [], 
    maxElement([H|T], Max, IdxMax),
    ivan([H|T], IdxMax, K).

ivan([K], 0, K).
ivan(L, Start, K):- 
    length(L, N), 
    nthRest(Killed, Start, L, LnoKilled),
    NewStart is (Start + 5) mod (N - 1), 
    ivan(LnoKilled, NewStart, K).

/* Задача 2
За просто число p ≥ 2, с F_p бележим полето с носител {0, 1, . . . , p − 1} с операциите събиране и умножение по модул p.
Представяне на вектор v ∈ F^n_p на пролог наричаме списък L_v с n елемента v1, v2, . . . , vn в този ред. 
Представяне на множество от вектори M ⊆ F^n_p на пролог ще наричаме списък L_M с |M| елемента, които представят различни вектори от M.
Да се дефинира предикат gen_bases(N, P, L, B), който по дадени естествено число N, просто число P и списък L от представяния на вектори v ∈ F^N_P, генерира в B всевъзможните представяния на базиси за F^N_P, чиито елементи са представени в L.
Забележка: Това е илюстрация на операциите събиране и умножение със скалар в F^n_p при
p = 5 и n = 3. В този случай имаме, че 3.(2, 4, 1) = (1, 2, 3), защото 3.2 = 6 ≡ 1 (mod 5),
3.4 = 12 ≡ 2 (mod 5) и 3.1 = 3 ≡ 3 (mod 5). Също така (1, 2, 3) + (4, 1, 4) = (0, 3, 2), защото
1 + 4 = 5 ≡ 0 (mod 5), 2 + 1 = 3 ≡ 3 (mod 5) и 3 + 4 = 7 ≡ 2 (mod 5).
Забележка: С операциите събиране и умножение по модул p, F^n_p е линейно пространство над това поле,
съответно базис са максимални по включване множества от линейно независими вектори в F^n_p.
*/

:-use_module(library(clpfd)).

sumModP(N, M, P, K):- K is (N + M) mod P.

multModP(N, M, P, K):- K is (N * M) mod P.

sumTwoNVectorsModP([], [], _, []).
sumTwoNVectorsModP([N|T1], [M|T2], P, [K|T3]):-
    sumModP(N, M, P, K),
    sumTwoNVectorsModP(T1, T2, P, T3).

scalarMultNVectorModP(_, [], _, []).
scalarMultNVectorModP(N, [M|T2], P, [K|T3]):-
    multModP(N, M, P, K),
    scalarMultNVectorModP(N, T2, P, T3).

coordinateMultNVectorWithNSetOfKVectorsModP([], [], _, []).
coordinateMultNVectorWithNSetOfKVectorsModP([AI|T1], [VI|T2], P, [AIxVI|T3]):-
    scalarMultNVectorModP(AI, VI, P, AIxVI),
    coordinateMultNVectorWithNSetOfKVectorsModP(T1, T2, P, T3).

sumKSetOfNVectorsModP([X], _, X).
sumKSetOfNVectorsModP([H|T], P, R):-
    T \= [],
    sumKSetOfNVectorsModP(T, P, R1),
    sumTwoNVectorsModP(R1, H, P, R).

genNNumbersleP(L, N, P):- 
    P1 #= P - 1,
    length(L, N),
    L ins 0..P1,
    label(L).

linearCombination(Vector, SetOfVectors, P, LinCombo):-
    coordinateMultNVectorWithNSetOfKVectorsModP(Vector, SetOfVectors, P, Res),
    sumKSetOfNVectorsModP(Res, P, LinCombo).

subsequence([], []).
subsequence([H|T], [H|R]):- subsequence(T, R).
subsequence([_|T], R):- subsequence(T, R).

notEmptySubsequence(L, S):- subsequence(L, S), S \= [].

isZeroNVector(L):- forall(member(X, L), X =:= 0).

atLeastOneNonZero(L):- member(X, L), X =\= 0.


/*
Let V be a vector space. A minimal set of vectors in V that spans V is called a basis for V.
Equivalently, a basis for V is a set of vectors that is linearly independent and spans V.
As a result, to check if a set of vectors form a basis for a vector space, 
one needs to check that it is linearly independent and that it spans the vector space. 
If at least one of these conditions fail to hold, then it is not a basis.
In the theory of vector spaces, a set of vectors is said to be linearly dependent if 
there is a nontrivial linear combination of the vectors that equals the zero vector.
If no such linear combination exists, then the vectors are said to be linearly independent.
*/
basisCondition(B, N, P):-
    length(B, K), 
    not((
        genNNumbersleP(KVector, K, P), 
        atLeastOneNonZero(KVector),
        linearCombination(KVector, KSetOfVectors, P, Res),
        isZeroNVector(Res)
    )), % linearly independent
    forall((genNNumbersleP(L, N, P)), 
                (notEmptySubsequence(B, KSetOfVectors), 
                length(KSetOfVectors, K), 
                genNNumbersleP(KVector, K, P),
                linearCombination(KVector, KSetOfVectors, P, L), % spans F^n_p
                write(L), nl, write(KVector), nl, write(KSetOfVectors), nl)). 

% За вариант 2 множество от N линейно независими вектори в това пространство пак е базис
gen_basis(N, P, L, B):-
    notEmptySubsequence(L, B), 
    basisCondition(B, N, P).
