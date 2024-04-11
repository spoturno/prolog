% Ejercicio 1
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


%