| numeros tienePares |
numeros := #(1 3 5 7 9).
tienePares := numeros anySatisfy: [:numero | numero even].
Transcript show: tienePares; nl.  "Devuelve false"
