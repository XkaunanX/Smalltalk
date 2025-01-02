| fileStream |
fileStream := FileStream fileNamed: 'mi_archivo.txt'.
fileStream nextPutAll: 'Hola, Mundo!'.
fileStream close.
