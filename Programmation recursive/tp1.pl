append2([], Ys, Ys).
append2([X|Xs], Ys, [X|Zs]) :- append2(Xs, Ys, Zs).

select2(A, [A|Ls], Ls).
select2(A, [X|Ls], [X|Rs]) :- select2(A, Ls, Rs).

reverse2([], []).
reverse2([X|Xs], Ys) :- reverse2(Xs, Zs), append2(Zs, [X], Ys).

reverse3(Xs, Ys) :- reverse_acc(Xs, Ys, []).

% reverse_acc(+Xs, -Ys, +Acc) est vrai si append(reverse(Xs), Acc, Ys).
reverse_acc([], Acc, Acc).
reverse_acc([X|Xs], Ys, Acc) :- reverse_acc(Xs, Ys, [X|Acc]).

% Exercice 1

homme(homer).
homme(bart).
femme(lisa).
femme(maggie).
pere(homer, bart).
pere(homer, lisa).
pere(homer, maggie).

enfant(X, Y) :- pere(Y, X).

fils(X, Y) :- enfant(X, Y), homme(X).
fille(X, Y) :- enfant(X, Y), femme(X).

frere_ou_soeur(X, Y) :- pere(P, X), pere(P, Y).

frere(X, Y) :- homme(X), frere_ou_soeur(X, Y).

soeur(X, Y) :- femme(X), frere_ou_soeur(X, Y).

% Exercice 4

min([X], X).
min([X|Xs], M) :- min(Xs, M2), min_2(X, M2, M).
min_2(M1, M2, M1) :- M1 =< M2.
min_2(M1, M2, M2) :- M1 > M2.

% Exercice 5
first(X, [X|_]).

% Exercice 6
my_last(A, [_, X|Xs]) :- my_last(A, [X|Xs]).
my_last(X, [X]).

% Exercice 7
my_member(X, [X|_]).
my_member(X, [Y|Xs]) :- X \= Y, my_member(X, Xs).

% Exercice 8
all_equals([]).
all_equals([_]).
all_equals([X,X|Xs]) :- all_equals([X|Xs]).

% Exercice 9
insert([], _, []).
insert([X|Xs], E, [X,E|Ys]) :- insert(Xs, E, Ys).

% Exercice 10
group([], []).
group([X,Y|Xs], [[X,Y]|Ys]) :- group(Xs, Ys).

% Exercice 11
palindrome(As) :- reverse3(As, Rs), Rs = As.