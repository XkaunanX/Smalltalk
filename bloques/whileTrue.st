| contador |
contador := 1.

"Se ejecuta mientras el contador sea menor o igual a 5"
contador <= 5 whileTrue: [
    Transcript show: 'Contador: ', contador printString; nl.
    contador := contador + 1.  "Incrementa el contador"
].
