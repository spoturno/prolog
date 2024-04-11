
nat(0).
nat(s(X)) :- nat(X).

suma(0, X, X).
suma(s(X), Y, s(S)) :- suma(X,Y,S).

% A partir de los elementos de L, obtener todos los pares de elementos que suman N
suman_N(L, N, [E1, E2]) :-
    member(E1, L),
    member(E2, L),
    N =:= E1 + E2.

suman_N2(L, N, [E1, E2]) :-
    select(E1, L, L1),
    select(E2, L1, _),
    N =:= E1 + E2.


puzzle([S,E,N,D,M,O,R,Y]):-
    % Generate
    permutation([1,2,3,4,5,6,7,8,9,0],[S,E,N,D,M,O,R,Y,_,_]),
    % Check
    Y is (D+E) mod 10,
    C1 is (D+E) // 10,
    E is (N+R+C1) mod 10,
    C2 is (N+R+C1) //10,
    N is (E+O+C2) mod 10,
    C3 is (E+O+C2) // 10,
    O is (S+M+C3) mod 10,
    M is (S+M+C3) // 10,
    M =\= 0,
    S =\= 0.


puzzle2([S,E,N,D,M,O,R,Y]):-
    % generate
    select(S,[1,2,3,4,5,6,7,8,9,0],L1),
    select(E,L1,L2),
    select(N,L2,L3),
    select(D,L3,L4),
    select(M,L4,L5),
    select(O,L5,L6),
    select(R,L6,L7),
    select(Y,L7,_),
    % check
    Y is (D+E) mod 10,
    C1 is (D+E) // 10,
    E is (N+R+C1) mod 10,
    C2 is (N+R+C1) //10,
    N is (E+O+C2) mod 10,
    C3 is (E+O+C2) // 10,
    O is (S+M+C3) mod 10,
    M is (S+M+C3) // 10,
    M =\= 0,
    S =\= 0.

% Al subir los checks recortamos ramas del arbol
puzzle3([S,E,N,D,M,O,R,Y]):-
    select(D,[1,2,3,4,5,6,7,8,9,0],L1),
    select(E,L1,L2),
    Y is (D+E) mod 10,
    select(Y,L2,L3),
    C1 is (D+E) // 10,
    select(N,L3,L4),
    select(R,L4,L5),
    E is (N+R+C1) mod 10,
    C2 is (N+R+C1) //10,
    select(O,L5,L6),
    N is (E+O+C2) mod 10,
    C3 is (E+O+C2) // 10,
    select(S,L6,L7),
    select(M,L7,_),
    O is (S+M+C3) mod 10,
    M is (S+M+C3) // 10,
    M =\= 0,
    S =\= 0.


% Genero una lista de elementos (variables sin instanciar) y en cada una instancio una lista de variables con N elementos.
columnas([],_).
columnas([X|Filas],N):-
    length(X,N),
    columnas(Filas,N).

% Problema del cuadrado latino
cuadro(CuadroF,M,N) :- length(CuadroF,M), columnas(CuadroF,N).

% Genero un cuadro de NxN y chequeo que es un cuadro latino
latino(N,Categorias,Latino):-
    % Generate
    cuadro(Latino,N,N),
    % Check
    latinoC(Latino,Categorias).

latinoC([],_).
latinoC([H|T],Cat):-
    permutation(Cat,H),
    latinoC(T,Cat),
    % La primera fila es compatible
    compatible(H,T).

compatible(_,[]).
compatible(X,[H|T]):-
    filas_compatibles(X,H),
    compatible(X,T).

filas_compatibles([],[]).
filas_compatibles([H|T],[H1|T1]):-
    H \= H1,
    filas_compatibles(T,T1).
