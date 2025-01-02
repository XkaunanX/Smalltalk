| vehiculo1 vehiculo2 |

"Crear un coche"
vehiculo1 := Coche new init: 'Toyota' modelo: 'Corolla'.

"Crear una bicicleta"
vehiculo2 := Bicicleta new init: 'Trek' modelo: 'Domane'.

"Ambos vehículos arrancan"
vehiculo1 arrancar.  "Llamará al método arrancar de la clase Coche"
vehiculo2 arrancar.  "Llamará al método arrancar de la clase Bicicleta"
