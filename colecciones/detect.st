| numeros encontrado |
numeros := #(1 2 3 4 5 6 7 8 9 10).
encontrado := numeros detect: [:numero | numero > 5].
Transcript show: encontrado; nl.
