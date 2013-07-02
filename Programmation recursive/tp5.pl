% Exercice 2
f(0, 0).
f(N, S) :- f_aux(N, [S|_]).

% f_aux(+N, -Ss) est vrai si Ss est l'ensemble des valeurs [f(n)..f(n)].
f_aux(0, [0]).
f_aux(N, [S|Prec]) :- N1 is N - 1, f_aux(N1, Prec),
               reverse(Prec, PrecRev), f_sum(0, N, Prec, PrecRev, S).

% f_sum(+N, +Prec, +PrecRev, -S) est vrai si S est la valeur de la somme de la
% fonction f pour i dans [I..N-1] si Prec contient les valeurs précédentes de f
% et PrecRev ces mêmes valeurs dans l'ordre d'inverse.
f_sum(N, N, _, _, 0) :- !.
f_sum(I, N, [P|Prec], [PR|PrecRev], S) :-
    I1 is I + 1, S1 is (P + PR + (I * I)) mod (10 + N + I),
    f_sum(I1, N, Prec, PrecRev, S2), S is S1 + S2.