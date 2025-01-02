| numeros cuadrados |
numeros := #(1 2 3 4 5).
cuadrados := numeros collect: [:numero | numero * numero].
Transcript show: cuadrados; nl.
