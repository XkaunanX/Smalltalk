| numeros suma |
numeros := #(1 2 3 4 5).
suma := numeros inject: 0 into: [:acumulado :numero | acumulado + numero].
Transcript show: suma; nl.
