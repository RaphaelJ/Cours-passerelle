homme(homer).
homme(bart).
femme(lisa).
femme(maggie).
pere(homer, bart).
pere(homer, lisa).
pere(homer, maggie).

enfant(X,Y) :- pere(Y,X).
enfant(X,Y) :- mere(Y,X).

fils(X,Y) :- enfant(X,Y), homme(X).
fille(X,Y) :- enfant(X,Y), femme(X).

frere_ou_soeur(X,Y) :- pere(Z, X), pere(Z,Y), X \= Y.
frere_ou_soeur(X,Y) :- mere(Z, X), mere(Z,Y), X \= Y.

frere(X,Y) :- frere_ou_soeur(X, Y), homme(X).
soeur(X,Y) :- frere_ou_soeur(X, Y), femme(X).

