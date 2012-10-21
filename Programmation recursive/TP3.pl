permutation([], []).
permutation([X|Xs], Ys) :- permutation(Xs, Zs), select(X, Ys, Zs).

