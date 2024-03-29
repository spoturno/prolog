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



% Ejercicio 3
% Defina los siguientes predicados en prolog
% suma(+L, ?S)          S es la suma de los elementos de la lista L
% pares(+L, ?P)         P es una lista conteniendo solo los elementos pares de la lista
% mayores(+L, +X, ?M)   M es una lista con los elementos de L que son mayores que X
% merge(+L1, +L2, ?L3)  L3 es el resultado de combinar ordenadamente los elementos de las listas (ordenadas) L1 y L2  

suma_acc([], Ac, Ac).
suma_acc([X|L], Ac, S) :-
    Ac1 is Ac + X, 
    suma_acc(L, Ac1, S).

suma(L, S) :- suma_acc(L, 0, S).


suma_v2([], 0).
suma_v2([Head|Tail], S) :-
    suma_v2(Tail, STail),
    S is Head + STail.


pares([], []).
pares([X|L1], [X|L2]) :-
    X1 is X mod 2,
    X1 =:= 0,
    pares(L1, L2).
pares([X|L], P) :-
    X1 is X mod 2,
    X1 =\= 0,
    pares(L, P).


mayores([], _, []).
mayores([T|H1], X, [T|H2]) :- 
    T > X,
    mayores(H1, X, H2).
mayores([T|H], X, M) :-
    T =< X,
    mayores(H, X, M).


merge([], L, L).
merge(L, [], L).
merge([T1|L1], [T2|L2], [T1|L3]) :-
    T1 =< T2,
    merge(L1, [T2|L2], L3).
merge([T1|L1], [T2|L2], [T2|L3]) :-
    T1 > T2,
    merge([T1|L1], L2, L3).
