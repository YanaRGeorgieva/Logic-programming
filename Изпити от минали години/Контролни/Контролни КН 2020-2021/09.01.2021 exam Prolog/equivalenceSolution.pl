p(R, S):- composition(R, S, C), condition(C).

% Нека изберем този дефиниция за композиция на две релации:
% R○S = {<a, c>| \exists b(<a,b> \in S and <b,c> \in R)}
composition([], _, []).
composition([H|T], S, Res):- 
    link(H, S, Res1),
    composition(T, S, Res2),
    append(Res1, Res2, Res).

link(_, [], []).
link([B, C], [[A, B]|ST], [[A, C]|Res]):-
    link([B, C], ST, Res).
link([B, C], [[A, D]|ST], [[A, C]|Res]):-
    B \= D,
    link([B, C], ST, Res).

reflexive(C) :- not(((member([X, _], C); member([_, X], C)), not(member([X, X], C)))).

symmetric(C):- not((member([X, Y], C), not(member([Y, X], C)))).

transitive(C):- not((member([X, Y], C), member([Y, Z], C), not(member([X, Z], C)))).

condition(C):- reflexive(C), symmetric(C), transitive(C).

