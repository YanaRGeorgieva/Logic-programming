parent(sasho, ani).
parent(ani, tosho).
parent(ani, kiro).

ancestor(X, Y):- parent(X, Y).
ancestor(X, Y):- parent(X, Z), ancestor(Z, Y).
% Пробвайте го така и ще видите, че при преудовлетворяване 
% ще задълбаете в една бездънна рекурсия ancestor(X, Y):- ancestor(Z, Y), patent(X, Z).


/* 1.
 append
 Използва се по дадени два списъка да получите конкатенацията им като нов трети.
 Също се използва по даден списък за направил разбиванията на този списък чрез преудовлетворяване( т.е. натискаме ; )
*/
% append(L1, L2, L3) where L3 = L1 + L2
append([], L2, L2).
append([Head|Tail], L2, [Head|Result]):- append(Tail, L2, Result).

/* Examples:
?- append([a, b], [c, d], C).
C = [a, b, c, d].

?- append(A, B, [a, b, c, d]).
A = [],
B = [a, b, c, d] ;
A = [a],
B = [b, c, d] ;
A = [a, b],
B = [c, d] ;
A = [a, b, c],
B = [d] ;
A = [a, b, c, d],
B = [] ;
false.

?- append(A, [c, d], [a, b, c, d]).
A = [a, b] ;
false.


?- append([a, b], B, [a, b, c, d]).
B = [c, d].

?- append([a, c], B, [a, b, c, d]).
false.
*/



