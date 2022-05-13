%Ejercicio 1.
pertenece([X|_], X). % Regresa true si el elemento es la cabeza de la lista.
pertenece([_|L], Y) :- pertenece(L,Y). % Es verdad si el elemento pertenece a la cola de la lista.

indice([X|_], X, 0). % El indice del elemento en la cabeza es 0.
indice([_|L], X, N) :- indice(L, X, M), N is M+1. %Es verdad si el indice del elemento en la cola de la lsita es N-1.

%Ejercicio 2.
%Relación que nos dice que nodos de la gráfica son adyacentes.
directo('Los Mochis', 'El Fuerte'). 
directo('El Fuerte', 'Los Mochis').
directo('El Fuerte', 'Bahuichivo').
directo('Bahuichivo', 'El Fuerte').
directo('Bahuichivo', 'Divisadero').
directo('Divisadero', 'Bahuichivo').
directo('Divisadero', 'Creel').
directo('Creel', 'Divisadero').

% a)
%Esto actúa como la cerradura transitiva de la relación directo. 
%Además, cómo la gráfica sólo tiene 5 vértices el camino no cíclico
%más largo es de longitud 4 (Consecuencia de la caracterización de
%Árboles en grafos).
llegar(X, Y) :- directo(X, Y); 
    (directo(X,Z), directo(Z, Y));
    (directo(X,A), directo(A,B), directo(B,Y));
    (directo(X,I), directo(I,J), directo(J,K), directo(K,Y)).

%Si, esta respuesta es tonta y no sirve fuera de este contexto.
lugares(X, Y, Z) :- 
    trim(X, ['Los Mochis', 'El Fuerte', 'Bahuichivo', 'Divisadero', 'Creel'], W), cut(Y, W, Z);
    trim(X, ['Creel', 'Divisadero', 'Bahuichivo', 'El Fuerte', 'Los Mochis'], V), cut(Y, V, Z).
%Esto reconoce los caminos, pero no los resuelve, y no sé por que.
%lugares(X, X, [X]).
%lugares(X, Y, [X, Y]) :- directo(X,Y).
%lugares(X, Y, [P|L]) :-
%    X \== P, % X y P deben de ser diferentes.
%    \+ pertenece(L, P), % No deben de haber repetidos en la lista.
%    directo(X, P), %Debe de haber un camino directo entra dos estaciones contiguas en la lista.
%    lugares(P, Y, L). %El resto de la lista debe de ser un camino entre la cabeza de la lista actual y el destino.

%Elimina todos los elementos antes de X en la lista.
trim(_, [], []).
trim(X, [X|XS], [X|XS]).
trim(X, [_|YS], L) :- trim(X, YS, L).

%Elimina todos los elementos de la lista que vayan después de X.
cut(X, Y, Z) :- trim(X, U, V), reversa(Y, U), reversa(Z, V).

%Hace la reversa de la lista.
reversa(X, Y) :- reversa(X, Y, []).
reversa([], X, X).
reversa([X|XS], Y, Z) :- reversa(XS, Y, [X|Z]).

