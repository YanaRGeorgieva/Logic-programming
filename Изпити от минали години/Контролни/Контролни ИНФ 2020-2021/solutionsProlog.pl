isLipschitz(L, C):- 
    not((member([X, FX], L), 
    member([Y, FY], L), 
    not( abs(FX-FY) =< C*abs(X-Y)))).

%[Title, Author]
gatherAuthors([], []).
gatherAuthors([[_, Author]|Rest], [Author|Res]):- 
    gatherAuthors(Rest, Res),
    not(member(Author, Res)).
gatherAuthors([[_, Author]|Rest], Res):-
    gatherAuthors(Rest, Res),
    member(Author, Res).

acceptableOrderOneAuthor(Books, Author, Shelves):-
    member(Shelf, Shelves),
    not((member([Title, Author], Books),
        not(member([Title, Author], Shelf)))).

acceptableOrder(Books, Authors, Shelf1, Shelf2, Shelf3):-   
    not((member(Author, Authors),
        not(acceptableOrderOneAuthor(Books, Author, [Shelf1, Shelf2, Shelf3])))).

averageDisbalance(Shelf1, Shelf2, Shelf3, AvgDiff):-
    length(Shelf1, N1),
    length(Shelf2, N2),
    length(Shelf3, N3),
    AvgDiff is (abs(N1-N2)+abs(N2-N3)+abs(N3-N1))/3.

splitBooks([], [], [], []).
splitBooks([Book|Rest], [Book|Shelf1], Shelf2, Shelf3):- 
    splitBooks(Rest, Shelf1, Shelf2, Shelf3).
splitBooks([Book|Rest], Shelf1, [Book|Shelf2], Shelf3):- 
    splitBooks(Rest, Shelf1, Shelf2, Shelf3). 
splitBooks([Book|Rest], Shelf1, Shelf2, [Book|Shelf3]):- 
    splitBooks(Rest, Shelf1, Shelf2, Shelf3).

commonElement(L1, L2):- member(X, L1), member(X, L2).

generateShelves(Books, Shelf1, Shelf2, Shelf3):- 
    splitBooks(Books, Shelf1, Shelf2, Shelf3),
    not(commonElement(Shelf1, Shelf2)),
    not(commonElement(Shelf2, Shelf3)),
    not(commonElement(Shelf3, Shelf1)).
 
condition(Books, Authors, Shelf1, Shelf2, Shelf3, AvgDiff):-
    acceptableOrder(Books, Authors, Shelf1, Shelf2, Shelf3),
    averageDisbalance(Shelf1, Shelf2, Shelf3, AvgDiff).

order(Books, Shelf1, Shelf2, Shelf3):-
    gatherAuthors(Books, Authors),
    generateShelves(Books, Shelf1, Shelf2, Shelf3),
    condition(Books, Authors, Shelf1, Shelf2, Shelf3, AvgDiff),
    not((generateShelves(Books, Shelf11, Shelf22, Shelf33),
        (Shelf1 \= Shelf11 ; Shelf2 \= Shelf22 ; Shelf3 \= Shelf33),
        condition(Books, Authors, Shelf11, Shelf22, Shelf33, AvgDiff2),
        AvgDiff2 < AvgDiff)). 
