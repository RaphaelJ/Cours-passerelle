bell(N, B) :- sterling_line(N, Xs), sum(Xs, B).

% sterling_line(+N, -Xs), vrai si Xs est la liste des nombre de Sterling de N
% éléments en K parties, pour K dans [1, N].
sterling_line(1, [1]) :- !.
sterling_line(N, Xs) :-
    N1 is N - 1, sterling_line(N1, Ps), sterling_next_line(Ps, Xs).

% sterling_next_line(+Ps, -Ns), vrai si Ns est la liste des nombres de Sterling
% pour N éléments en K parties, pour K dans [1, N] et Ps la liste des nombres de
% Sterling pour N-1 éléments.
sterling_next_line(Ps, [1|Ns]) :- sterling_next_line_aux(Ps, 2, Ns).

% sterling_next_line_aux(+Ps, +Ik, -Ns), vrai si Ns est la liste des nombres de
% Sterling pour N éléments en K parties avec K dans [Ik, N] (avec Ik > 1) et Ps
% la liste des nombres de Sterling pour N-1 éléments en K parties avec K dans
% [Ik-1, N-1].
sterling_next_line_aux([X, Y|Ps], Ik, [N|Ns]) :-
    N is X + Ik * Y, Ik1 is Ik + 1, sterling_next_line_aux([Y|Ps], Ik1, Ns), !.
sterling_next_line_aux(_, _, [1]).

% sum(+Xs, -N), vrai si N est la somme des éléments de Xs.
sum([], 0).
sum([X|Xs], N1) :- sum(Xs, N), N1 is N + X.