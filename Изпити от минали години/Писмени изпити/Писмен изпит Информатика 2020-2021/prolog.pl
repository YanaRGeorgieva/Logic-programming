slice(L, Begin, End, N, S):- slice(L, Begin, End, N, S, 0).

slice(_, _, End, _, [], Count):- Count > End.
slice([H|T], Begin, End, N, [H|R], Count):-
    Count >= Begin , Count =< End,
    Count mod N =:= 0,
    Count1 is Count + 1,
    slice(T, Begin, End, N, R, Count1).
slice([_|T], Begin, End, N, R, Count):-
    (Count < Begin;
    (Count >= Begin , Count =< End,
    Count mod N =\= 0)),
    Count1 is Count + 1,
    slice(T, Begin, End, N, R, Count1).

chocolatePalace(Blocks, Umpas, UsedUmpas, RedPoints):-
    makeTrip(Blocks, Umpas, UsedUmpas, RedPoints),
    length(UsedUmpas, N),
    not((makeTrip(Blocks, Umpas, UsedUmpas1, _),
        length(UsedUmpas1, N1),
        N1 > N)).

% chocolatePalace([[1,3],[2,3]],[[1,1],[2,2],[3,5], [4,5], [5,1], [6,1], [7,1]],L,N).

makeTrip([], _, [], 0).
makeTrip([[_, BlockKG]|TailBlocks], Umpas, UsedUmpas, RedPoints):-
    subsequence(Umpas, BlockUmpas),
    canCarry(BlockKG, BlockUmpas),
    allUseful(BlockKG, BlockUmpas),
    length(BlockUmpas, BlockRedPoints),
    makeTrip(TailBlocks, Umpas, TailUsedUmpas, TailBlocksRedPoints),
    add(TailUsedUmpas, BlockUmpas, UsedUmpas),    
    RedPoints is TailBlocksRedPoints + BlockRedPoints.

add(L, [], L).
add(L, [H|T], [H|R]):- add(L, T, R), not(member(H, R)).
add(L, [H|T], R):- add(L, T, R), member(H, R).

subsequence([], []).
subsequence([H|T], [H|R]):- subsequence(T, R).
subsequence([_|T], R):- subsequence(T, R).

canCarry(BlockKG, BlockUmpas):- 
    sumSecond(BlockUmpas, Strength), Strength >= BlockKG.

sumSecond([], 0).
sumSecond([[_,H]|T], N):- sumSecond(T, M), N is H + M.

remove(X, L, R):- append(A, [X|B], L), append(A, B, R).

allUseful(BlockKG, BlockUmpas):- 
    not((remove(_, BlockUmpas, LessBlockUmpas), 
        canCarry(BlockKG, LessBlockUmpas))).

