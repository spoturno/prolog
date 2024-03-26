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