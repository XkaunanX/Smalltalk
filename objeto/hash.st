Object subclass: #Libro
    instanceVariableNames: 'titulo autor'.

Libro >> init: unTitulo autor: unAutor
    titulo := unTitulo.
    autor := unAutor.

Libro >> hash
    ^(titulo hash + autor hash) \\ 97.

| libro1 libro2 |
libro1 := Libro new init: 'El Quijote' autor: 'Cervantes'.
libro2 := Libro new init: 'El Quijote' autor: 'Cervantes'.
Transcript show: (libro1 hash = libro2 hash) printString; nl.  "Esto imprimirá 'true'"
