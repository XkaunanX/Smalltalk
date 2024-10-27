"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Farmacia
	instanceVariableNames: 'nombre conjRem'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Farmacia guid: (GUID fromString: '{5581E204-7EF5-4602-94C7-C8846679A54E}')!
Farmacia comment: ''!
!Farmacia categoriesForClass!Kernel-Objects! !
!Farmacia methodsFor!

agregar:unRem
conjRem add:unRem.!

eliminar:unRem
conjRem remove: unRem ifAbsent:[^nil].!

esVacio
^conjRem isEmpty.!

existe: unRem
^conjRem includes: unRem.!

init: nom

nombre:= nom.
conjRem := OrderedCollection new.!

modNombre:otroNom

nombre:=otroNom.!

recuperar: i
^conjRem at: i.!

tamanio
^conjRem size.!

verNombre
^nombre!

verTodos
^conjRem.! !
!Farmacia categoriesFor: #agregar:!public! !
!Farmacia categoriesFor: #eliminar:!public! !
!Farmacia categoriesFor: #esVacio!public! !
!Farmacia categoriesFor: #existe:!public! !
!Farmacia categoriesFor: #init:!public! !
!Farmacia categoriesFor: #modNombre:!public! !
!Farmacia categoriesFor: #recuperar:!public! !
!Farmacia categoriesFor: #tamanio!public! !
!Farmacia categoriesFor: #verNombre!public! !
!Farmacia categoriesFor: #verTodos!public! !

!Farmacia class methodsFor!

construct:alias
"construyo o instancio un objeto de la clase farmacia  y la inicializo con su nombre
checkear lo del new!!!!!!!!
construct no actua directamente, solo salva el parametro para darle el primer valor a la variable de instacia llamada nombre
^ significa que devuelvo algo
self dice que camino toma para ejecutar el metodo en este caso el mismo (objeto receptor o el apuntado y se mantiene al ir saltando entre clases)
super lo revota para arriba (osea a la super clase inmediata de la clase que esta siendo apuntada)
new significa que forjo segun la plantilla
init es el metodo de clase que inicializa el objeto porque al instanciarlo me intereza instanciarlo con valores"
^self new init:alias.!

crear:nom

^self new init:nom.
! !
!Farmacia class categoriesFor: #construct:!public! !
!Farmacia class categoriesFor: #crear:!public! !

