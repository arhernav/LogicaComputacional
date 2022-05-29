%-----------------------------------------------------Ejercicio 1-------------------------------------------------------
%Convertidor de ascii a binario implementando una base de datos
    binary(a, '01100001').
    binary(b, '01100010').
    binary(c, '01100011').
    binary(d, '01100100').
    binary(e, '01100101').
    binary(f, '01100110').
    binary(g, '01100111').
    binary(h, '01101000').
    binary(i, '01101001').
    binary(j, '01101010').
    binary(k, '01101011').
    binary(l, '01101100').
    binary(m, '01101101').
    binary(n, '01101110').
    binary(o, '01101111').
    binary(p, '01110000').
    binary(q, '01110001').
    binary(r, '01110010').
    binary(s, '01110011').
    binary(t, '01110100').
    binary(u, '01110101').
    binary(v, '01110110').
    binary(w, '01110111').
    binary(x, '01111000').
    binary(y, '01111001').
    binary(z, '01111010').

binaryValue(A, Z):-
    binary(A, Z); binary(Z, A).


procList([X|Y], Solucion):-
    maplist(binaryValue, [X|Y], Solucion).

ejercicio1([X|Y]):-
    procList([X|Y], Solucion), print(Solucion).

%---------------------------------------------------Ejercicio 2-------------------------------------------------------------

esCubo(gris).
esCubo(rojo).
esCubo(azul).
esCubo(verde).
esCubo(amarillo).
esCubo(rosa).

:- dynamic debajo/2.
debajo(rojo, gris).
debajo(azul, rojo).
debajo(rosa, amarillo).

esBot(X):-
    \+ debajo(_, X).

sobre(X,Y):-
    debajo(Y, X).

hastaArriba(X):-
    \+ debajo(X, _).

bloqueado(X):-
    debajo(X, _).


hastaAbajo(X):-
    (esBot(X) -> print(X));(debajo(Y, X) -> hastaAbajo(Y)).


mover(X, Y):-
    debajo(X,Y), retract(debajo(X,Y)), assertz(debajo(Y,X)).

%---------------------------------------------------Ejercicio 3-------------------------------------------------------------
aceptar(S) :- afn(q0, S).

afn(q1, []).
afn(q2, []).
afn(Qi, [X|XS]) :- transicion(Qi, X, Qs), afn(Qs, XS).

transicion(q0, "b", q1).
transicion(q0, "b", q2).
transicion(q0, "b", q3).
transicion(q1, "a", q1).
transicion(q1, "b", q1).
transicion(q2, "a", q1).
transicion(q3, "b", q0).

%---------------------------------------------------Ejercicio 4-------------------------------------------------------------
mezclar([], [], []).
mezclar(L1, [], L1).
mezclar([], L2, L2).
mezclar([X|L1], [Y|L2], [X|LMerge]) :- (X @=< Y), mezclar(L1, [Y|L2], LMerge), !.
mezclar([X|L1], [Y|L2], [Y|LMerge]) :- (Y @=< X), mezclar([X|L1], L2, LMerge), !.

%Esta es una implementación de MergeSort.
ordenar([], []).
ordenar([X], [X]).
ordenar(L, S) :-
    divorcio(L, P, M), ordenar(P, Po),
    ordenar(M, Mo), mezclar(Po, Mo, S), !.

%Desde mi primer segundo semestre, siempre he llamado divorcio a la función de separación en mis implementaciones de MergeSort.
divorcio([], [], []).
divorcio([X], [X], []).
divorcio([X, Y|XYS], [X|XS], [Y|YS]) :- divorcio(XYS, XS, YS).
