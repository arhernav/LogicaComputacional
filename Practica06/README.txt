README


- Juan Carlos Zenteno Pompa 316251608
- Hernández Navarro Armando 317340347



Comentarios
Ejercicio 1

Ejercicio 2
- En el ejercicio 2, la  funcion mover(X, Y) funciona de manera que no se puede mover el cubo X encima del Y, si el Y no esta directamente encima del X, y ademas no se verifica si el cubo Y esta bloqueado o no. Esto se implementa de esta manera ya que asi es como se entiende el pdf.
Seria posible implementarlo de manera que se verifique si el cubo Y esta bloqueado o que no sea necesario que el cubo Y este previamente encima del X, pero se tomaron las condiciones que se entendieron en el pdf.
 
Ejercicio 3
- La solución de este ejercicio fue trivial una vez que volvimos a leer la definición formal de un AFN.

Ejercicio 4
- La implementación de este ejercicio consistión en traducir a Prolog la función de mezcla, la cuál es sencillísima. El operador de corte se usa al final de los casos recursivos para evitar el backtracking, así, evitamos que existan varias respuestas iguales para la mezcla de dos listas donde ambas tienen al menos un elemento en común cuando se hace una consulta. 
