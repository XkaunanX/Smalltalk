| numeros pares |
numeros := #(1 2 3 4 5 6 7 8 9 10).
pares := numeros select: [:numero | numero even].
Transcript show: pares; nl.
