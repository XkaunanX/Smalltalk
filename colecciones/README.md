# Colecciones en Smalltalk

Smalltalk ofrece una variedad de colecciones que permiten almacenar y manipular conjuntos de datos de manera eficiente. A continuación se describen las colecciones más comunes en Smalltalk.

## Colecciones Básicas

### array.st

`Array` es una colección básica que almacena elementos en un orden específico. Los elementos se acceden mediante índices enteros. Un `Array` en Smalltalk es una colección con un tamaño fijo, es decir, el tamaño del array no cambia una vez creado.

### orderedCollection.st

`OrderedCollection` es una colección similar a `Array`, pero con la capacidad de modificar su tamaño dinámicamente. Permite agregar, quitar o modificar elementos en cualquier momento, sin necesidad de crear una nueva colección.

### queue.st

`Queue` es una colección basada en la estructura de cola (FIFO - First In, First Out). Los elementos se agregan al final de la cola y se extraen del principio, lo que la hace útil para gestionar procesos o tareas en orden de llegada.

### set.st

`Set` es una colección que almacena elementos únicos, es decir, no permite duplicados. Un `Set` se utiliza cuando se necesita garantizar que no haya elementos repetidos y no importa el orden en que se agregan.

### sortedCollection.st

`SortedCollection` es una colección que mantiene sus elementos ordenados automáticamente. A medida que se agregan elementos, se colocan en su posición correcta según un criterio de orden especificado.

### dictionary.st

`Dictionary` es una colección que almacena pares de clave-valor. Cada valor se asocia a una clave única, lo que permite la búsqueda eficiente de datos mediante la clave.

### bag.st

`Bag` es una colección que almacena elementos sin importar el orden, pero permite elementos duplicados. A diferencia de un `Set`, en un `Bag` se pueden agregar varios elementos con el mismo valor, lo que puede ser útil cuando el conteo de ocurrencias de un elemento es importante.

## Métodos Comunes en las Colecciones

### allSatisfy.st

`allSatisfy.st` es un método que evalúa si todos los elementos de una colección cumplen con un determinado predicado. Devuelve `true` si todos los elementos cumplen con la condición, de lo contrario devuelve `false`.

### anySatisfy.st

`anySatisfy.st` es un método que evalúa si al menos un elemento de la colección cumple con un determinado predicado. Devuelve `true` si al menos uno de los elementos cumple con la condición, de lo contrario devuelve `false`.

### collect.st

`collect.st` es un método que permite transformar todos los elementos de una colección aplicando una operación o función a cada uno de ellos. Devuelve una nueva colección con los elementos transformados.

### detect.st

`detect.st` es un método que busca un elemento en la colección que cumpla con un predicado determinado. Si encuentra un elemento que cumple con el predicado, lo devuelve; de lo contrario, devuelve `nil`.

### findFirst.st

`findFirst.st` es un método similar a `detect.st`, pero específicamente diseñado para encontrar el primer elemento que cumpla con el predicado. Si no se encuentra ninguno, devuelve `nil`.

### injectInto.st

`injectInto.st` es un método que realiza una operación acumulativa sobre los elementos de la colección. Comienza con un valor inicial y aplica una función a cada elemento, acumulando el resultado.

### reject.st

`reject.st` es un método que permite filtrar los elementos de una colección. Devuelve una nueva colección que contiene todos los elementos que **no** cumplen con el predicado especificado.

### select.st

`select.st` es un método que filtra los elementos de una colección. Devuelve una nueva colección con los elementos que cumplen con el predicado especificado.

### size.st

`size.st` es un método que devuelve el número de elementos en la colección.

# Jerarquia de clases

```plaintext
Object
   |
   |
   +-- Collection
         |
         +-- IndexedCollection
         |     |
         |     +-- FixedSizeCollection
         |           +-- Array
         |           +-- String
         |
         +-- OrderedCollection
         |     +-- SortedCollection
         |
         +-- Bag
         +-- Set
               +-- Dictionary

```
