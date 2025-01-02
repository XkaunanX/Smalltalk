Object subclass: #Coche
    instanceVariableNames: 'marca modelo año velocidad'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''.

"Código de inicialización"
Coche >> init: unaMarca modelo: unModelo año: unAño
    marca := unaMarca.
    modelo := unModelo.
    año := unAño.
    velocidad := 0.  "Velocidad inicial es 0"

"Modificar la velocidad"
Coche >> acelerar: unaVelocidad
    velocidad := velocidad + unaVelocidad.

"Frenar"
Coche >> frenar
    velocidad := velocidad - 10.  "Reduce la velocidad en 10 unidades"
    velocidad := velocidad < 0 ifTrue: [0] ifFalse: [velocidad].

"Ver velocidad actual"
Coche >> verVelocidad
    ^velocidad asInteger.

"Imprimir información del coche"
Coche >> detalles
    Transcript show: 'Marca: ', marca; nl.
    Transcript show: 'Modelo: ', modelo; nl.
    Transcript show: 'Año: ', año printString; nl.
    Transcript show: 'Velocidad actual: ', velocidad printString; nl.
