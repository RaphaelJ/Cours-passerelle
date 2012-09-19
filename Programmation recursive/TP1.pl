first([X|_], X).

second([X,Y|_], X, Y).
second([X,Y|_], Y, X).