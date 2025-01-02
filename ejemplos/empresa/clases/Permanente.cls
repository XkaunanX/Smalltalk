"Filed out from Dolphin Smalltalk 7"!

Empleado subclass: #Permanente
	instanceVariableNames: 'antiguedad'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Permanente guid: (GUID fromString: '{FB185B05-59E1-41D9-BF49-168051CA1036}')!
Permanente comment: ''!
!Permanente categoriesForClass!Kernel-Objects! !
!Permanente methodsFor!

basico 
"Retorna el sueldo basico del empleado permanente"
^300*horas + (antiguedad * (300 * horas* 0.10)).!

init:unNom con:ch con:cantH con:unaLic con:unEst con: unaAnt
super init: unNom con: ch con: cantH con: unaLic con: unEst.
antiguedad:=unaAnt.!

modAntig:unaAnt
"Modifica la antigüedad por unaAnt"
antiguedad:=unaAnt.!

verAntig 
"Retorna la antigüedad del empleado"
^antiguedad.! !
!Permanente categoriesFor: #basico!public! !
!Permanente categoriesFor: #init:con:con:con:con:con:!public! !
!Permanente categoriesFor: #modAntig:!public! !
!Permanente categoriesFor: #verAntig!public! !

!Permanente class methodsFor!

crear:unNom con: ch con:cantH con: unaLic con: unEst con: unaAnt
^(super new) init:unNom con:ch con:cantH con:unaLic con:unEst con: unaAnt.
! !
!Permanente class categoriesFor: #crear:con:con:con:con:con:!public! !

