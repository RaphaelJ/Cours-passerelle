% Liste les permutations uniques
permut([], []).
permut(Xs, [X|Ys]) :- select([X, C], Xs, Zs), C > 1, C1 is C-1,
                      permut([[X, C1]|Zs], Ys).
permut(Xs, [X|Ys]) :- select([X, C], Xs, Zs), C =:= 1, permut(Zs, Ys).

numper(L, N) :- findall(P, permut(L, P), R), length(R, N).
