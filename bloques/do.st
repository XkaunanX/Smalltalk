| numeros |
numeros := #(1 2 3 4 5).

"Itera sobre cada elemento de la colección"
numeros do: [:numero |
    Transcript show: 'Número: ', numero printString; nl.
].
