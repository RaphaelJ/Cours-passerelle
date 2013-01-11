% Exercice 2
partition([], []).
partition(Xs, [[X]|Ps]) :- select(X, Xs, Rs), partition(Rs, Ps).
partition([X|Xs], [[X|P]|Ps2]) :- partition(Xs, Ps), select(P, Ps, Ps2).

% Exercice 3
union([], Ys, Ys).
union([X|Xs], Ys, [X|Zs]) :- not(member(X, Ys)), union(Xs, Ys, Zs).
union([X|Xs], Ys, Zs) :- member(X, Ys), union(Xs, Ys, Zs).

% Exercice 4
is_bintree(Tr) :- not(is_list(Tr)).
is_bintree([Left,Right]) :- is_bintree(Left), is_bintree(Right).

% Exercice 5
count_leaves([L,R], S) :- count_leaves(L, LS), count_leaves(R, RS)
                        , S is LS + RS, !.
count_leaves(_, 1).

% Exercice 6
depth_tree([L, R], N) :- depth_tree(L, LD), depth_tree(R, RD)
                       , max_2(LD, RD, M), N is M + 1, !.
depth_tree(_, 0).

max_2(M1, M2, M1) :- M1 >= M2.
max_2(M1, M2, M2) :- M1 < M2.

% Exercice 7
parcours([L, R], Xs) :- parcours(L, Ls), parcours(R, Rs), append(Ls, Rs, Xs), !.
parcours(N, [N1]) :- N1 is N + (N mod 2).

% Exercice 9
simplify([L, R], T) :- simplify(L, L2), simplify(R, R2)
                     , simplify_aux([L2, R2], T), !.
simplify(T, T).

simplify_aux([A, A], A) :- not(is_list(A)), !.
simplify_aux(T, T).
