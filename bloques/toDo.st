| contador |
contador := 1.

"Se ejecuta mientras el contador sea menor o igual a 5"
contador to: 5 do: [:i |
    Transcript show: 'Contador: ', i printString; nl.
].