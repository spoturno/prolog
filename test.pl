
nat(0).
nat(s(X)) :- nat(X).

suma(0, X, X).
suma(s(X), Y, s(S)) :- suma(X,Y,S).
