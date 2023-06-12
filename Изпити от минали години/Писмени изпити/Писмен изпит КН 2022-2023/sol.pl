:-use_module(library(clpfd)).
% length, nth са пренаписани с #.
s(M):- pairs(A, B), length(L, 3 * A), L ins 0..B, isSpecific(L, A).

isSpecific(L, N):- Nm1 is N - 1, 
    forall( 
        (K in 0..Nm1, label([K]), nth(K, L, Ak)), 
            (
                ( 1 #=< Ak + 6, Ak + 6 #=< 3 * N, 
                    nth(Ak + 6, L, R), nth(3 * K, L, R))
                    ;
                ((1 #> Ak + 6; Ak + 6 #> 3 * N), 
                    nth(3 * N - K, L, R), nth(3 * K, L, 3 * R + 1)) 
            )
        ).

p(L1, L2):- 
    compose(L2, L1, Res), 
    compose(L1, R, Res2), 
    compose(R, L2, Res3), 
    areEqualSets(Res2, Res3).

areEqualSets(A, B):- 
	forall(member(X, A), member(X, B)),
	forall(member(X, B), member(X, A)).

compose([], B, []).
compose(A, [], []).
compose([[A1, C1|T1]], B, [[A1, B1]|R]):- 
	member([C1, B1], B), compose(T1, B, R).
