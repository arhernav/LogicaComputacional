README


- Juan Carlos Zenteno Pompa 316251608
- Hernández Navarro Armando 317340347



Solución
Ejercicio 1
- Una vez creada la base de  datos que almacena como hechos tuplas  de la letra y su traducción a binario, podemos resolver el problema por medio de dos reglas. La primer regla, "binaryValue()", se encarga de verificar si recibe el codigo binario correspondiente a una letra o una letra, y devolver el elemento faltante de su tupla, la segunda regla, "procList()" , se encarga de reecibir una lista de letras o de cadenas correspondientes a codigo binario y mapear la regla binaryValue() a cada elemento.

Al final se agrego una tercera regla "ejercicio1()" que se encarga de mejorar la presentacion del problema al recibir solo una lista. El problema podria ser resuelto unicamente con procList()

Ejercicio 2
- Se definen los cubos como constantes y se crean las relaciones a partir de las cuales se van a trabajar. De estas relaciones "debajo(X, Y)" es en la que nos basamos para modelar el ejercicio.

Inciso 1: Se crea una regla recursiva que revisa la posicion del cubo recibido. Si el cubo  recibido es el cubo de hasta abajo en su pila, entonces lo devuelve. Si el cubo recicbido no es el de hasta abajo, entonces ejecuta una regla recursiva para bajar hata encontrar el cubo de mas abajo.

Inciso 2: En esta regla se crea una función que cambia la posición de dos cubos. Como condición se tiene que solo se pueden alternar la posición de dos cubos si uno esta debajo del otro. Esta regla modifica la base de datos temporalmente, pero los cambios son eliminados al salir del programa debido a que asi se solicito en la practica.
 
Ejercicio 3
- La solución de este ejercicio fue trivial una vez que volvimos a leer la definición formal de un AFN.

Ejercicio 4
- La implementación de este ejercicio consistión en traducir a Prolog la función de mezcla, la cuál es sencillísima. El operador de corte se usa al final de los casos recursivos para evitar el backtracking, así, evitamos que existan varias respuestas iguales para la mezcla de dos listas donde ambas tienen al menos un elemento en común cuando se hace una consulta. 



Comentarios
- En el ejercicio 2, la  funcion mover(X, Y) funciona de manera que no se puede mover el cubo X encima del Y si el cubo Y no esta directamente encima del X. Ademas no se verifica si el cubo Y esta bloqueado o no. Esto se implementa de esta manera ya que asi es como se entiende el pdf.
Seria posible implementarlo de manera que se verifique si el cubo Y esta bloqueado o que no sea necesario que el cubo Y este previamente encima del X, pero se tomaron las condiciones que se entendieron en el pdf.
