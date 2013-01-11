% Exercice 1
remove_last([_], []).
remove_last([X, Y|Xs], [X|Ys]) :- remove_last([Y, Xs], Ys).

% Exercice 2
rep([], []).
rep([X], [X]).
rep([X, X|Xs], Ys) :- rep([X|Xs], Ys).
rep([X, Y|Xs], [X|Ys]) :- X \= Y, rep([Y|Xs], Ys).

% Exercice 3
sub(_, _, [], []).
sub(A, B, [A|Xs], [B|Ys]) :- sub(A, B, Xs, Ys).
sub(A, B, [X|Xs], [X|Ys]) :- X \= A, sub(A, B, Xs, Ys).

% Exercice 4
rem_odd([], []).
rem_odd([_], []).
rem_odd([_,Y|Xs], [Y|Ys]) :- rem_odd(Xs, Ys).

% Exercice 5
pack([], []).
pack([X|Xs], [Ys|Zs]) :- pack_group([X|Xs], Ys, Rs), pack(Rs, Zs).

% pack_group(+Xs, -Ys, -Rs) est vrai si Ys est le plus long préfixe de Xs
% d'éléments identiques et Rs ce qui suit le préfixe dans Xs. Xs est une liste
% non vide
pack_group([X], [X], []).
pack_group([X, X|Xs], [X|Ys], Rs) :- pack_group([X|Xs], Ys, Rs).
pack_group([X, Y|Xs], [X], [Y|Xs]) :- X \= Y.

% Exercice 6
skip3([], []).
skip3([A], [A]).
skip3([A,B], [A,B]).
skip3([A,B,_], [A,B]).
skip3([A,B,_,_], [A,B]).
skip3([A,B,_,_,_|Xs], [A,B|Ys]) :- skip3(Xs, Ys).

% Exercice 7
encode([], []).
encode([X|Xs], [[X,N]|Zs]) :- encode_group([X|Xs], N, Rs), encode(Rs, Zs).

% encode_group(+Xs, -N, -Rs) est vrai si N est la longueur du plus long préfixe 
% de Xs d'éléments identiques et Rs ce qui suit le préfixe dans Xs.
% Xs est une liste non vide
encode_group([_], 1, []).
encode_group([X, X|Xs], N1, Rs) :- encode_group([X|Xs], N, Rs), N1 is N + 1.
encode_group([X, Y|Xs], 1, [Y|Xs]) :- X \= Y.

% Exercice 8
decode([], []).
decode([[_,0]|Xs], Ys) :- decode(Xs, Ys).
decode([[X,C]|Xs], [X|Ys]) :- C > 0, C1 is C - 1, decode([[X,C1]|Xs], Ys).

% Exercice 9
skipN(_, _, [], []).
skipN(N, M, Xs, Ys) :- split_at(N, Xs, Ps, Rs), drop(M, Rs, Rs2)
                     , skipN(N, M, Rs2, Zs), append(Ps, Zs, Ys).

% split_at(+N, +Xs, -Ys, -Rs) est vrai si Ys est les N premiers éléments de Xs
% et Rs les éléments restants de Xs (après Ys).
split_at(0, Xs, [], Xs).
split_at(_, [], [], []).
split_at(N, [X|Xs], [X|Ys], Rs) :- N > 0, N1 is N - 1, split_at(N1, Xs, Ys, Rs).

% drop(+M, +Xs, -Ys) est vrai si Ys est la liste Xs privée de ses M premiers
% éléments
drop(0, Xs, Xs).
drop(_, [], []).
drop(M, [_|Xs], Ys) :- M > 0, M1 is M - 1, drop(M1, Xs, Ys).

% Exercice 10
prime(N) :- not(prime_go(N, 2)).

% prime_go(+N, +I) est vrai si un nombre entier compris dans [I; SQRT(N)] divise
% N.
prime_go(N, I) :- I * I =< N, N mod I =:= 0.
prime_go(N, I) :- I * I =< N, I1 is I + 1, prime_go(N, I1).