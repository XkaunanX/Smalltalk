"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Libro
	instanceVariableNames: 'isbn titulo autor editorial estado dni'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Libro guid: (GUID fromString: '{308C61D0-E62B-4EBA-BADC-F3B13ABDE966}')!
Libro comment: ''!
!Libro categoriesForClass!Kernel-Objects! !
!Libro methodsFor!

iniciar:unIsbn titulo: unTitulo autor: unAutor editorial: unEditorial estado: unEstado dni: unDni
isbn := unIsbn.
titulo := unTitulo.
autor := unAutor.
editorial := unEditorial.
estado := unEstado.
dni := unDni.
!

modAutor: unString
autor := unString.!

modDni: unInteger
dni := unInteger.!

modEditorial: unString
editorial := unString.!

modEstado: unBool
estado := unBool.!

modIsbn: unInteger
isbn := unInteger.!

modTitulo: unString
titulo := unString.!

verAutor
^autor.!

verDni
^dni.!

verEditorial
^editorial.!

verEstado
^estado.!

verIsbn
^isbn.!

verTitulo
^titulo.! !
!Libro categoriesFor: #iniciar:titulo:autor:editorial:estado:dni:!public! !
!Libro categoriesFor: #modAutor:!public! !
!Libro categoriesFor: #modDni:!public! !
!Libro categoriesFor: #modEditorial:!public! !
!Libro categoriesFor: #modEstado:!public! !
!Libro categoriesFor: #modIsbn:!public! !
!Libro categoriesFor: #modTitulo:!public! !
!Libro categoriesFor: #verAutor!public! !
!Libro categoriesFor: #verDni!public! !
!Libro categoriesFor: #verEditorial!public! !
!Libro categoriesFor: #verEstado!public! !
!Libro categoriesFor: #verIsbn!public! !
!Libro categoriesFor: #verTitulo!public! !

!Libro class methodsFor!

crear:unIsbn titulo: unTitulo autor: unAutor editorial: unEditorial estado: unEstado dni: unDni
^self new iniciar:unIsbn titulo: unTitulo autor: unAutor editorial: unEditorial estado: unEstado dni: unDni.! !
!Libro class categoriesFor: #crear:titulo:autor:editorial:estado:dni:!public! !

