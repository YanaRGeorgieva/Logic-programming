operation(1, A, B, C):- C is A + B.
operation(2, A, B, C):- C is A * B.
operation(3, A, B, C):- C is A mod B.
operation(4, A, B, C):- C is A - B.

p(K, X):- member([A, _, _], X), pathK(X, A, A, K).

pathK(_, _, Accum, K):- 
	0 =:= Accum - K.
pathK(X, A, Accum, K):- 
	member([A, B, Op], X),
	operation(Op, Accum, B, NewAccum), 
	pathK(X, B, NewAccum, K).

:-use_module(library(clpfd)).
ro([], [], S):- S #= 0.
ro([AI|AR], [BI|BR], S):- 
	ro(AR, BR, SR), 
	S #= SR + abs(AI - BI).

d(D, K, V):- 
	member(X, D), length(X, N), !, 
	length(V, N), 
	V ins 0..1,
	label(V),
	findall(X, (member(X, D), ro(V, X, S), S mod 2 #= 0, label([S])), L), 
	length(L, K1), 
	K1 #< K.