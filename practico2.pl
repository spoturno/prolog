% Ejercicio 2a
% Defina los siguientes predicados sin utilizar acumuladores
% largo(+L, ?N)     N es el largo de la lista Lprpra
% maximo(+L,?M)     M es el máximo elemento de la lista L

largo([], 0).
largo([_|T], N) :-
    largo(T, N2),
    N is N2 + 1.

maximo([X], X) :- !.
maximo([X|T], X) :- maximo(T, M), X > M.
maximo([X|T], M) :- maximo(T,M), X =< M.


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
    0 =:= X mod 2,
    pares(L1, L2).
pares([X|L], P) :-
    0 =\= X mod 2,
    pares(L, P).


mayores([], _, []).
mayores([T|H1], X, [T|H2]) :- 
    T > X,!,
    mayores(H1, X, H2).
mayores([T|H], X, M) :-
    T =< X,!,
    mayores(H, X, M).


merge([], L, L):- !.
merge(L, [], L):- !.
merge([T1|L1], [T2|L2], [T1|L3]) :-
    T1 =< T2, !,
    merge(L1, [T2|L2], L3).
merge([T1|L1], [T2|L2], [T2|L3]) :-
    T1 > T2, !,
    merge([T1|L1], L2, L3).


% Ejercicio 4a
% Defina los siguientes predicados en Prolog:
% insertionsort(+L,?S) S es el resultado de ordenar la lista L utilizando el algoritmo insertion sort
% mergesort(+L,?S) S es el resultado de ordenar la lista L utilizando el algoritmo merge sort
% quicksort(+L,?S) S es el resultado de ordenar la lista L utilizando el algoritmo quick sort

insertionsort([], []).
insertionsort([H|T], Sorted):-
    insertionsort(T, SortedTail),
    insert(H, SortedTail, Sorted).

insert(X, [], [X]).
insert(X, [H|T], [X, H|T]) :- X =< H.
insert(X, [H|T1], [H|T2]) :- X > H, insert(X, T1, T2).


split([], [], []).
split(L, A, B) :-
    append(A, B, L),
    largo_v2(A, N),
    largo_v2(B, N).

mergesort([], []).  
mergesort([X, Y], [X, Y]) :- X =< Y.
mergesort([X, Y], [Y, X]) :- X > Y.
mergesort(L, S) :-
    split(L, A, B),
    mergesort(A, S1),
    mergesort(B, S2),
    merge(S1, S2, S).



quicksort([],[]).

quicksort([H|T], Sorted) :-
    partition(T, H, Less, Greater),
    quicksort(Less, SortedLess),
    quicksort(Greater, SortedGreater),
    append(SortedLess, [H|SortedGreater], Sorted).

partition([], _, [], []).
partition([X|Xs], Pivot, [X|Less], Greater) :-
    X < Pivot,
    partition(Xs, Pivot, Less, Greater).
partition([X|Xs], Pivot, Less, [X|Greater]) :-
    X >= Pivot,
    partition(Xs, Pivot, Less, Greater).



% Ejercicio 5
% Considere la representación de vectores mediante listas de valores reales en Prolog. Implemente los siguientes predicados:
% neg(+V, ?W) donde W es el vector opuesto a V
% suma(+V, +W, ?T) donde T es la suma de los vectores V y W
% dot(+V, +W, ?P) donde P es el producto punto entre V y w
% dist(+V, +W, ?D) D es la distancia euclídea entre V y W

neg([], []).
neg([X|L], [N|L2]) :- N is -X, neg(L, L2).

suma([], [], []).
suma([X1|L1], [X2|L2], [X3|L3]) :-
    X3 is X1 + X2,
    suma(L1, L2, L3).

dot([], [], 0).
dot([X1|L1], [X2|L2], P) :-
    dot(L1, L2, P3),
    P is P3 + X1*X2.

dist(V1, V2, D) :-
    suma_dif_cuadrados(V1, V2, D2),
    D is sqrt(D2).
   
% función auxiliar para dist
suma_dif_cuadrados([], [], 0).
suma_dif_cuadrados([X1|L1], [X2|L2], D) :-
    suma_dif_cuadrados(L1, L2, D2),
    D is D2 + (X2 - X1)**2.
    

    
% Ejercicio 6
% Considere la representación de matrices mediante listas de listas de valores reales en Prolog.
% Implemente los siguientes predicados:
% columna(+M,?C,?R) C es la primera columna de M en forma de lista, R  es M sin la primera columna.
% transpuesta(+M,?T) T es la transpuesta de la matriz T
% simetrica(+M) M es una matriz simétrica
% suma(+M,+N,?S) S es la suma de las matrices M y N
% producto(+M,+N,?P) P es el producto de las matrices M y N

columna([], [], []).
columna([[H|T]|Rows], [H|Cs], [T|Rs]) :-
    columna(Rows, Cs, Rs).

transpuesta([], []).
transpuesta(M, [C|T]) :-
    columna(M, C, R), % Extraer primer columna y el resto de la matriz
    transpuesta(R, T).

% Una matriz es simetrica si es igual a su transpuesta.
simetrica(M) :-
    transpuesta(M, T),
    M = T.

suma_matrices([], [], []).
suma_matrices([R1|Rs1], [R2|Rs2], [S|Ss]) :-
    suma(R1, R2, S), % Sum the corresponding rows
    suma_matrices(Rs1, Rs2, Ss).

producto(M, N, P) :-
    transpuesta(N, NT),
    dot(M, NT, P).
