"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Biblioteca
	instanceVariableNames: 'nombre coleccionLibros'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Biblioteca guid: (GUID fromString: '{A33FCDDA-09AA-4EEC-BE31-39B954C6914E}')!
Biblioteca comment: ''!
!Biblioteca categoriesForClass!Kernel-Objects! !
!Biblioteca methodsFor!

agregarLibro: unLibro
coleccionLibros add:unLibro.!

buscarLibro: unLibro
coleccionLibros includes: unLibro.!

cantidadLibros
^coleccionLibros size.!

eliminarLibro: unLibro
coleccionLibros remove:unLibro.!

esVacia
^coleccionLibros isEmpty.!

iniciar:unNombre
nombre := unNombre.
coleccionLibros := OrderedCollection new.!

modNombre: unString
nombre := unString.!

recuperarLibro: unIndice
^coleccionLibros at:unIndice.!

verNombre
^nombre.!

verTodos
^coleccionLibros.! !
!Biblioteca categoriesFor: #agregarLibro:!public! !
!Biblioteca categoriesFor: #buscarLibro:!public! !
!Biblioteca categoriesFor: #cantidadLibros!public! !
!Biblioteca categoriesFor: #eliminarLibro:!public! !
!Biblioteca categoriesFor: #esVacia!public! !
!Biblioteca categoriesFor: #iniciar:!public! !
!Biblioteca categoriesFor: #modNombre:!public! !
!Biblioteca categoriesFor: #recuperarLibro:!public! !
!Biblioteca categoriesFor: #verNombre!public! !
!Biblioteca categoriesFor: #verTodos!public! !

!Biblioteca class methodsFor!

crear: unNombre
^self new iniciar:unNombre.! !
!Biblioteca class categoriesFor: #crear:!public! !

