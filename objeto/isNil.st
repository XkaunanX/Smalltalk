| objeto1 objeto2 |
objeto1 := nil.
objeto2 := 'Hola'.

Transcript show: (objeto1 isNil) printString; nl.  "Esto imprimirá 'true'"
Transcript show: (objeto2 isNil) printString; nl.  "Esto imprimirá 'false'"
