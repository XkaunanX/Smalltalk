| numeros todosPositivos |
numeros := #(1 2 3 4 5).
todosPositivos := numeros allSatisfy: [:numero | numero > 0].
Transcript show: todosPositivos; nl.  "Devuelve true"
