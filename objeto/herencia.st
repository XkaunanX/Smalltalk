| perro gato |

"Crear un perro"
perro := Perro new init: 'Rex' edad: 5.

"Crear un gato"
gato := Gato new init: 'Whiskers' edad: 3.

"Ver detalles de ambos animales"
perro detalles.  "Llamará al método detalles de Perro"
gato detalles.   "Llamará al método detalles de Gato"

"Los perros ladran"
perro ladrar.

"Los gatos maullan"
gato maullar.
