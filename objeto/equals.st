Object subclass: #Persona
    instanceVariableNames: 'nombre edad'.

Persona >> init: unNombre edad: unaEdad
    nombre := unNombre.
    edad := unaEdad.

Persona >> equals: otroObjeto
    ^(otroObjeto isKindOf: Persona) and: [(nombre = otroObjeto nombre) and: (edad = otroObjeto edad)].

| persona1 persona2 |
persona1 := Persona new init: 'Juan' edad: 30.
persona2 := Persona new init: 'Juan' edad: 30.
Transcript show: (persona1 equals: persona2) printString; nl.  "Esto imprimirá 'true'"
