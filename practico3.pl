% ==== Ejercicio 1 ====
% El mapa se representa por una lista de regiones de la forma: 
% region(Nombre, Color, ColoresVacios)
% Implementar colorear(Mapa, Colores) Mapa se encuentra coloreado con Colores de forma tal que no hay dos vecinos iguales

colores_disponibles([rojo, verde, azul, amarillo, marron, violeta]).

color_valido_para_vecinos(_, []).
color_valido_para_vecinos(Color, [ColorVecino | RestoVecinos]) :- 
    colores_disponibles(ColoresDisponibles),
    member(ColorVecino, ColoresDisponibles),
    Color \= ColorVecino,
    color_valido_para_vecinos(Color, RestoVecinos).

colorear([], _).
colorear([region(_, Color, Vecinos) | RestoMapa], Colores) :-
    member(Color, Colores),
    color_valido_para_vecinos(Color, Vecinos),
    colorear(RestoMapa, Colores).


% ==== Ejercicio 2 ====
/* El problema del «ataque de las k reinas» consiste en
distribuir k reinas en un tablero de n por n, de forma que
toda casilla del tablero quede atacada por una reina, y
ninguna reina sea atacada por otra.
*/
% Defina el siguiente predicado:
% kreinas(K,N,Reinas) donde Reinas es una  solución al problema del ataque de las K reinas en un tablero de tamaño N por N

kreinas(_, N, Reinas) :-
    N > 0,
    primeros(N, Ns),
    permutacion(Ns, Reinas),
    seguras(Reinas).

seguras([]).
seguras([R|Rs]) :-
    no_ataca(R, Rs),
    seguras(Rs).

% no_ataca(+R, +Rs)
% La reina R no está siendo atacada por ninguna de las Rs
% Y también la reina R no ataca a ninguna de las Rs
no_ataca(R, Rs) :- no_ataca(R, 1, Rs).
no_ataca(_,_,[]).
no_ataca(R, N, [R1|Rs]) :-
    R =\= R1 + N,
    R =\= R1 - N,
    N1 is N+1,
    no_ataca(R,N1,Rs).

largo([], 0).
largo(L, N) :- largo(L, 0, N).

largo([], Ac, Ac).
largo([_|Lista], Ac, N) :-
    Ac1 is Ac + 1,
    largo(Lista, Ac1, N).

% primeros(+N, ?Ns) <- Ns es una lista con elementos de 1..N
primeros(0, []).
primeros(N, P) :- primeros(N, [], P).

% primeros(+N, +Ac, ?P)
% Predicado acumulador donde se va acumulando en Ac los numeros hasta ese punto
primeros(0, Ac, Ac).
primeros(N, Ac, P) :-
    N > 0,
    N1 is N-1,
    primeros(N1, [N|Ac], P).

% permutación(+Lista, ?Perm)
% Perm es una permutacion de elementos de Lista
permutacion([], []).
permutacion([H| Lista], Perm) :-
    permutacion(Lista, P),
    select(H, Perm, P).

% Se puede mejorar usando un acumulador de reinas agregadas que son seguras.