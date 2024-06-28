% suma(X,Y,S) <- S es la suma de X e Y
suma(0, N, N).
suma(s(N), Y, s(S)) :- suma(N, Y, S).


% resta(X, Y, R) <- R es la diferencia entre X e Y (X=Y+R)
resta(N, 0, N).
resta(s(X), Y, s(R)) :- resta(X, Y, R). 


% producto(X,Y,P) <- P es el producto entre X e Y (P = X*Y)
producto(0, _, 0).
producto(X, s(Y), P) :-
    producto(X, Y, Temp),
    suma(X, Temp, P).


% distintos(X,Y) <- X e Y son distintos
distintos(s(_), 0).
distintos(0, s(_)).
distintos(s(X), s(Y)) :- distintos(X,Y).


% mayor(X, Y) <- X es mayor que Y
mayor(s(_), 0).
mayor(s(X), s(Y)) :- mayor(X,Y).


% factorial(X, Y) <- Y es igual al factorial de X (Y = X!).
factorial(s(N), F) :- 
    factorial(N, R),
    producto(s(N), R, F).


% potencia(X,Y,Z) <- Z es igual a X elevado a la Y (Z = X^Y).
potencia(_, 0, s(0)).
potencia(X, s(0), X).
potencia(X, s(N), R) :- 
    potencia(X, N, Temp),
    producto(X, Temp, R).

    
/* largo(L, N) <- La lista L tiene N elementos (siendo N=s^n(0)). */
largo([], 0).
largo([_|T], s(N)) :- largo(T, N).

/* ultimo(L, X) <- X es el último elemento de la lista L. */
ultimo([X], X).
ultimo([_|T], X) :- ultimo(T, X).

/* sin_ultimo(L, S) <- S es la lista que se obtiene de suprimir el último elemento de L. */
sin_ultimo([_], []).
sin_ultimo([H|T], [H|T1]) :- sin_ultimo(T, T1).

/* reverso(L, R) <- La lista R contiene los elementos de la lista L en orden inverso */
reverso([], []).
reverso([H|T], L) :- 
    reverso(T, T1), 
    append(T1, [H], L).

/* subsecuencia(L, S) <- La lista S contiene elementos (no necesariamente consecutivos) de la lista L. Estos elementos preservan el orden de aparición que poseen en L. */
subsecuencia(_,[]).
subsecuencia([X|L], [X,S]) :- subsecuencia(L,S).
subsecuencia([_|T], S) :- subsecuencia(T,S).

/* sublista(L,S) <- La lista S contiene elementos consecutivos de la lista L. Estos elementos preservan el orden de aparición que poseen en L. */ 
sublista(_, []).
sublista(L, S) :-
    append(P, _, L),
    append(_, P, S).

/* prefijo(L, P) <- donde la lista P es prefijo de la lista L */
prefijo(_, []).
prefijo([X|T], [X|S]) :- prefijo(T,S).

/* sufijo(L, S) <- La lista S es un sufijo de la lista L. */
sufijo(X, X).
sufijo(L, S) :- append(_, S, L).
sufijo([_|L], S) :- sufijo(L,S).

/* borrar_todos(L,X,B) <- La lista B es la lista L sin ocurrencias del elemento X. */
borrar_todos([], _, []).
borrar_todos([X|T], X, B) :- borrar_todos(T, X, B).
borrar_todos([H|T], X, [H|B]) :- 
    dif(X, H),
    borrar_todos(T, X, B).

/* sin_repetidos(L,S) <- La lista S es la lista L sin elementos repetidos. (Suponga que L es una lista de naturales definidos como en el ejercicio 2). */
% Actualmente está dando error
sin_repetidos([], []).

sin_repetidos([H|T], S) :- 
    member(H,S),
    sin_repetidos(T,S).

sin_repetidos([H|T], [H|S]) :- 
    not(member(H,S)),
    sin_repetidos(T,S).


% Ejercicio 4b
conjunto([]).
conjunto([X|L]) :-
    not(member(X,L)),
    conjunto(L).

subconjunto(_, []).
subconjunto(C, [X|L]) :-
    member(X,C),
    subconjunto(C, L).

conj_iguales([], []).
conj_iguales(C1, C2) :-
    subconjunto(C1, C2),
    subconjunto(C2, C1),
    conjunto(C1),
    conjunto(C2).

interseccion([], _, []).
interseccion([X|T], C2, [X|S]) :-
    member(X, C2), !,
    interseccion(T, C2, S).
interseccion([_|T1], L2, S) :- interseccion(T1, L2, S).


union([], X, X).
union([H|T], C2, U) :-
    member(H, C2), !,
    union(T, C2, U).
union([H|T], C2, [H|U]) :- union(T, C2, U). 


% Ejercicio 5
sumaLista([],0).
sumaLista([0|T], Sum) :- sumaLista(T, Sum).
sumaLista([s(H)|T], s(Sum)) :- sumaLista([H|T], Sum).

% Ejercicio 6
progenitor(juan, jose).
progenitor(jose, pedro).
progenitor(pedro, maria).

ancestro(X, X, [X]).
ancestro(X, Y, [X|L]) :- 
    progenitor(X, Z),
    ancestro(Z, Y, L).


% % Ejercicio 8
%
% sigma(S) :- conjunto(S). 
%
% exp_reg(S, R) :-
%     sigma(S),
%     es_exp_regular(S, R).
%
% es_exp_regular(_, 0).
% es_exp_regular(_, e).
% es_exp_regular(S, [X]) :- member(X, S).
% es_exp_regular(S, (R1|R2)) :- es_exp_regular(S,R1), es_exp_regular(S,R2). % unión
% es_exp_regular(S, (R1-R2)) :- es_exp_regular(S, R1), es_exp_regular(S, R2). % concatencación 
% es_exp_regular(S, (R*)) :- es_exp_regular(S, R). % clausura de kleene
%
