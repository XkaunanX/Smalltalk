# Bloques y Condicionales en Smalltalk

En Smalltalk, no existen estructuras condicionales tradicionales como en otros lenguajes. En su lugar, el flujo de control se maneja mediante el uso de bloques y mensajes. A continuacion, se describen los bloques mas comunes y su funcionamiento en Smalltalk.

## Bloques en Smalltalk

Los bloques en Smalltalk son fragmentos de codigo que pueden ser almacenados y ejecutados posteriormente. Funcionan de manera similar a las funciones o lambdas en otros lenguajes de programacion. Los bloques son tratados como objetos, lo que significa que pueden ser pasados como argumentos, almacenados en variables o retornados desde otros bloques.

## `ifTrue`

El mensaje `ifTrue` en Smalltalk es una forma de manejar condicionales. Aunque Smalltalk no tiene una estructura de control `if` tradicional, `ifTrue` permite ejecutar un bloque solo si una condicion es verdadera. El mensaje se envia a un objeto, y si este objeto es considerado verdadero (por ejemplo, `true` o cualquier objeto distinto de `nil` o `false`), entonces se ejecuta el bloque asociado.

## `whileTrue`

El mensaje `whileTrue` permite ejecutar un ciclo en Smalltalk. Este mensaje se envia a un objeto, y mientras la condicion sea verdadera, el bloque de codigo asociado se ejecuta repetidamente. Al igual que `ifTrue`, `whileTrue` no utiliza una estructura de bucle tradicional, sino que depende del envio de un mensaje a un objeto para determinar si el ciclo debe continuar.

## Otros Bloques en Smalltalk

### `block.st`

Los bloques en Smalltalk son objetos que encapsulan un fragmento de codigo. Pueden ser almacenados, pasados entre objetos y ejecutados cuando sea necesario. Esta capacidad hace que los bloques sean muy flexibles y puedan utilizarse para diversas tareas como callbacks o manipulacion de datos.

### `do.st`

El mensaje `do` se envia a colecciones y ejecuta un bloque de codigo para cada elemento dentro de la coleccion. En lugar de usar una estructura de bucle tradicional, `do` permite iterar sobre los elementos de la coleccion y realizar operaciones sobre ellos.

### `toDo.st`

El mensaje `toDo` se utiliza para representar una tarea que debe ejecutarse. Este bloque se almacena como parte de una lista de tareas pendientes o como parte de un flujo de trabajo que debe completarse en el futuro.

