| numeros primerPar |
numeros := #(1 2 3 4 5 6).
primerPar := numeros findFirst: [:numero | numero even].
Transcript show: primerPar; nl.  "Devuelve 2"
