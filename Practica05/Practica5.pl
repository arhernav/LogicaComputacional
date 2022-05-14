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

    % B)
%Si, esta respuesta es tonta y no sirve fuera de este contexto.
lugares(X, Y, Z) :- 
    trim(X, ['Los Mochis', 'El Fuerte', 'Bahuichivo', 'Divisadero', 'Creel'], W), cut(Y, W, Z);
    trim(X, ['Creel', 'Divisadero', 'Bahuichivo', 'El Fuerte', 'Los Mochis'], V), cut(Y, V, Z).
%Esto reconoce los caminos, pero no los resuelve, y no sé por que.
coso(X, X, [X]).
coso(X, Y, [X, Y]) :- directo(X,Y).
coso(X, Y, [X, Z|L]) :-
    X \== Z, % X y Z deben de ser diferentes.
    \+ pertenece(L, Z), % No deben de haber repetidos en la lista.
    directo(X, Z), %Debe de haber un camino directo entre dos estaciones contiguas en la lista.
    coso(Z, Y, L). %El resto de la lista debe de ser un camino entre la cabeza de la lista actual y el destino.

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

% c) Los dos incisos anteriores ya cumplen con esto.
%Ejercicio 3.
primo(N) :- primo(N, 2).
primo(N, X) :- 
    X >= N; %Caso base. N nunca es divisible entre un numero mayor a sí mismo. Y un primo siempre es divisible entre sí mismo. 
    ( %Caso recursivo.
        \+ 0 is N mod X, %Solo es verdad si X no divide a N.
        primo(N, X+1) % Repetimos sobre X+1, es decir, vemos si el número siguiente también cumple esta propiedad.
    ).

%Ejercicio 4.
elimina_dups([], []).
elimina_dups([X|XS], ND) :- 
    elimina(X, XS, T), 
    elimina_dups(T, T1), 
    ND=[X|T1], !.
    
%Relaciona una lista con otra lista que tiene los mismos elementos, en el mismo orden, pero, sin ninguna instancia del elemento X.
elimina(_,[],[]).
elimina(X,[X|T],S) :- elimina(X, T, S).
elimina(X,[H|T],[H|T1]) :- elimina(X,T,T1).

%Ejercicio 5.
gusta_leer('Alexia').
gusta_leer('David').

lee('Alexia', 'Alicia en el país de las maravillas.').
lee('David', _) :- clima(nuboso);clima(lluvioso).

odia('Alexia', lluvioso).

:- dynamic
    clima/1.
%Ejercicio 6.
%Estas son las premisas. Nos estamos refiriendo a las personas por su relación más directa con el autor.
conyuge(autor, viuda).
conyuge(viuda, autor).
conyuge(padre, hijastra).
conyuge(hijastra, padre).

progenitor(padre, autor).
progenitor(viuda, hijastra).

tutor(X, Y) :- 
    progenitor(X, Y);
    progenitor(Z, Y), conyuge(X, Z).

abuelo(X, Y) :- tutor(Z, Y), tutor(X, Z).
