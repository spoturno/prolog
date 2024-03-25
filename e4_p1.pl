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



