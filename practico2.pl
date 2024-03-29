% Ejercicio 2a
% Defina los siguientes predicados sin utilizar acumuladores
% largo(+L, ?N)     N es el largo de la lista Lprpra
% maximo(+L,?M)     M es el máximo elemento de la lista L

largo([], 0).
largo([_|T], N) :-
    largo(T, N2),
    N is N2 + 1.

maximo([X], X).
maximo([X|T], X) :-
    maximo(T, M),
    X > M.
maximo([X|T], M) :-
    maximo(T,M),
    X =< M.

% Ejercicio 2b
% Defina los predicados de la parte b utilizando acumuladores

largo_acc([], Ac, Ac).
largo_acc([_|L], Ac, N) :-
    Ac1 is Ac + 1,
    largo_acc(L, Ac1, N).

largo_v2(L, N) :- largo_acc(L, 0, N).


maximo_acc([], Ac, Ac).
maximo_acc([X|L], Ac, M) :-
    X > Ac,
    maximo_acc(L, X, M).
maximo_acc([X|L], Ac, M) :-
    X =< Ac,
    maximo_acc(L, Ac, M).

maximo_v2([X|L], M) :- maximo_acc(L, X, M).

% ?- time(maximo_v2([1,2,3,4,10], M)).
% 8 inferences, 0.000 CPU in 0.000 seconds (92% CPU, 455789 Lips)
% M = 10 ;
% 10 inferences, 0.000 CPU in 0.000 seconds (82% CPU, 415386 Lips)
% false.

% ?- time(maximo([1,2,3,4,10], M)).
% 88 inferences, 0.000 CPU in 0.000 seconds (89% CPU, 1637240 Lips)
% M = 10 ;
% 7 inferences, 0.000 CPU in 0.000 seconds (80% CPU, 318312 Lips)
% false.


