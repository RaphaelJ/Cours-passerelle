% Exercice 3
perfect(N) :- sum_div(N, 1, S), S1 is S - N, S1 = N.

% sum_div(N, I, S) est vrai si S est la somme des diviseurs distincts dans
% [I..N].
sum_div(N, I, I) :- I * I =:= N.
sum_div(N, I, 0) :- I * I > N.
sum_div(N, I, S) :- I * I < N, M is N mod I, M \= 0, I1 is I + 1
                  , sum_div(N, I1, S).
sum_div(N, I, S) :- I * I < N, M is N mod I, M = 0, I1 is I + 1
                  , I2 is N / I, sum_div(N, I1, S1), S is I + I2 + S1.

% Exercice 4
my_flatten([], []).
my_flatten([X|Xs], Ys)     :- is_list(X), my_flatten(Xs, Zs), append(X, Zs, Ys).
my_flatten([X|Xs], [X|Ys]) :- not(is_list(X)), my_flatten(Xs, Ys).

% Exercice 5
sublist(_, []).
sublist(Xs, Ys) :- is_prefix(Ys, Xs).
sublist([_|Xs], Ys) :- sublist(Xs, Ys).

% is_prefix(+Ps, +Xs) est vrai si Ps est un préfixe de Xs.
is_prefix([], _).
is_prefix([X|Ps], [X|Xs]) :- is_prefix(Ps, Xs).

% Exercice 6
plateau([], []).
plateau(Xs, Ps) :- plateau_group(Xs, Ps1, Rs), plateau(Rs, Ps2)
                 , plateau_aux(Ps1, Ps2, Ps).

% plateau_aux(+Xs, +Ys, -Zs) est vrai si Zs est la plus longue liste entre Xs et
% Ys.
plateau_aux(Xs, Ys, Xs) :- length(Xs, LXs), length(Ys, LYs), LXs >= LYs.
plateau_aux(Xs, Ys, Ys) :- length(Xs, LXs), length(Ys, LYs), LXs < LYs.

% plateau_group(+Xs, -Ys, -Rs) est vrai si Ys est le plus long préfixe de Xs
% d'éléments identiques et Rs ce qui suit le préfixe dans Xs. Xs est une liste
% non vide
plateau_group([X], [X], []).
plateau_group([X, X|Xs], [X|Ys], Rs) :- plateau_group([X|Xs], Ys, Rs).
plateau_group([X, Y|Xs], [X], [Y|Xs]) :- X \= Y.