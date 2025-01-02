| diccionario |
diccionario := Dictionary new.
diccionario at: 'clave1' put: 'valor1'.
diccionario at: 'clave2' put: 'valor2'.
Transcript show: diccionario at: 'clave1'; nl.  "Devuelve 'valor1'"
