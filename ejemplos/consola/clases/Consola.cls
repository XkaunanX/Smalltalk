"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Consola
	instanceVariableNames: 'nombre libre usado c_juegos'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Consola guid: (GUID fromString: '{B80EB983-8F84-4D74-BB75-5331E2DFCDB9}')!
Consola comment: ''!
!Consola categoriesForClass!Kernel-Objects! !
!Consola methodsFor!

agregarJuego:unJuego
"Añado al final de la coleccion un nuevo juego"
c_juegos add:unJuego.!

elminarJuego:unJuego
"Elimino un juego de la coleccion o devuelve nil si no esta incluido en la coleccion"
c_juegos remove:unJuego ifAbsent:[^nil].!

existe:unJuego
"Devulve un juego si exista en la coleccion"
^c_juegos includes: unJuego.!

init:unNombre memoria:unEspacio
"Nombre"
nombre:=unNombre.
"Espacio total de la consola"
libre:=unEspacio.
"Espacio utilizado. *Inicializa en cero"
usado:=0.
"Coleccion de juegos"
c_juegos:= OrderedCollection new.!

isEmpty
"Devuelve verdadero si esta vacio"
^c_juegos isEmpty.!

modLibre:unInteger
"Modifico el espacio maximo"
libre:=unInteger.!

modNombre:unString
"Cambia el nombre de la consola"
nombre:=unString.!

modUsado:unInteger
"Modifica el espacio utilizado por los juegos"
usado:=unInteger.!

recuperarJuego:indice
"Devuelve un juego asociado a un indice"
^c_juegos at:indice.!

tamanio
"Devuelve la cantidad de juegos"
^c_juegos size.!

verLibre
"Devuelve el espacio libre"
^libre.!

verNombre
"Devuelve el nombre"
^nombre.!

verTodos
"Devuelve la coleccion"
^c_juegos.!

verUsado
"Devuelve el espacio usado"
^usado.! !
!Consola categoriesFor: #agregarJuego:!public! !
!Consola categoriesFor: #elminarJuego:!public! !
!Consola categoriesFor: #existe:!public! !
!Consola categoriesFor: #init:memoria:!public! !
!Consola categoriesFor: #isEmpty!public! !
!Consola categoriesFor: #modLibre:!public! !
!Consola categoriesFor: #modNombre:!public! !
!Consola categoriesFor: #modUsado:!public! !
!Consola categoriesFor: #recuperarJuego:!public! !
!Consola categoriesFor: #tamanio!public! !
!Consola categoriesFor: #verLibre!public! !
!Consola categoriesFor: #verNombre!public! !
!Consola categoriesFor: #verTodos!public! !
!Consola categoriesFor: #verUsado!public! !

!Consola class methodsFor!

crear:unNombre memoria:unaMemoria
"Constructor de objetos"
^self new init:unNombre memoria:unaMemoria.! !
!Consola class categoriesFor: #crear:memoria:!public! !

