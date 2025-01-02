| bloque suma |
bloque := [:a :b | a + b].
suma := bloque value: 3 value: 4.
Transcript show: 'La suma es: ', suma printString; nl.  "Esto imprimirá 'La suma es: 7'"
