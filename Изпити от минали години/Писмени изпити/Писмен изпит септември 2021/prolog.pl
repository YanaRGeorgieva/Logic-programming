% Problem 1, variant 1
is_multiplicative(L):- 
    not((
        remove([S1, W1], L, L1),
        remove([S2, W2], L1, L2),
        remove([S3, W3], L2, L3),
        not(commonElement(S1, S2, _)),
        not((member(X1, S1), 
            member(X2, S2), 
            not(member([X1, X2], S3))
            )),
        not((
            member([X1, X2], S3), 
            not((member(X1, S1), 
                member(X2, S2)))
            )),        
        W1 * W2 =\= W3
    )).

remove(X, L, R):- 
    append(A, [X|B], L), 
    append(A, B, R).

commonElement(L1, L2, X):- 
    member(X, L1), 
    member(X, L2).


% Problem 2, variant 1
% Since the sum of the rows and columns must be exactly N, then each list from D has exactly one apperence in the matrix (as a column ^ as a row).
generate_crossword(D, C):-
    length(D, N),
    subsequencePerm(D, S),
    transpose(S, TS),
    append(S, TS, RCS),
    equalSets(RCS, D),
    sameLength(D, M),
    length(S, SL),
    M + SL =:= N.

transpose([], []).
transpose(Matrix, TMatrix):- 
    Matrix = [H|_],
    length(H, NumColumns),
    transpose(NumColumns, Matrix, TMatrix).

transpose(0, _, []).
transpose(N, Rows, [Col|RestCols]):-
    N > 0, N1 is N - 1,
    takeFirstElementsOfRows(Rows, Col, NewRows),
    transpose(N1, NewRows, RestCols).

takeFirstElementsOfRows([], [], []).
takeFirstElementsOfRows([[First|RestRow]|OtherRows], [Fisrt|RestCol], [RestRow|NewRestRows]):-
    takeFirstElementsOfRows(OtherRows, RestCol, NewRestRows).

sameLength([H|T], M):- 
    length(H, M), 
    not((
        member(X, T), 
        not(length(X, M))
        )).

equalSets(A, B):- 
    subset(A, B), 
    subset(B, A).

subset(A, B):- 
    not((
        member(X, A), 
        not(member(X, B))
        )).

subsequencePerm(L, R):- 
    subsequence(L, S), 
    permutation(S, R).
    
subsequence([],[]).
subsequence([H|T], [H|R]):- 
    subsequence(T, R).
subsequence([_|T], R):- 
    subsequence(T, R).




