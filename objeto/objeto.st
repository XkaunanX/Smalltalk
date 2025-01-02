| miCoche |

"Crear un objeto de la clase Coche"
miCoche := Coche new init: 'Toyota' modelo: 'Corolla' año: 2020.

"Ver la velocidad inicial"
Transcript show: 'Velocidad inicial: ', miCoche verVelocidad printString; nl.

"Acelerar el coche"
miCoche acelerar: 30.
Transcript show: 'Después de acelerar: ', miCoche verVelocidad printString; nl.

"Frenar el coche"
miCoche frenar.
Transcript show: 'Después de frenar: ', miCoche verVelocidad printString; nl.

"Ver detalles del coche"
miCoche detalles.
