Object subclass: #Animal.
Object subclass: #Perro
    inherits: #Animal.

| miAnimal miPerro |
miAnimal := Animal new.
miPerro := Perro new.

Transcript show: (miPerro isKindOf: Animal) printString; nl.  "Esto imprimirá 'true'"
Transcript show: (miAnimal isKindOf: Perro) printString; nl.  "Esto imprimirá 'false'"
