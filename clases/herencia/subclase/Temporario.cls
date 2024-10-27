"Filed out from Dolphin Smalltalk 7"!

Empleado subclass: #Temporario
	instanceVariableNames: 'contrato'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Temporario guid: (GUID fromString: '{D197B204-CBAE-4EDC-891A-89CB52F379B1}')!
Temporario comment: ''!
!Temporario categoriesForClass!Kernel-Objects! !
!Temporario methodsFor!

basico
"Retorna el sueldo basico del empleado temporario"
^200*horas!

init:unNom con: ch con:cantH con:unaLic con:unEst con: unCont
"Inicializa la clase Temporario"
super init:unNom con: ch con:cantH con:unaLic con:unEst.
contrato:=unCont.!

modCont:unCont
"Modifica el contrato por unCont"
contrato := unCont.!

verCont
"Retorna el contrato del empleado"
^contrato.! !
!Temporario categoriesFor: #basico!public! !
!Temporario categoriesFor: #init:con:con:con:con:con:!public! !
!Temporario categoriesFor: #modCont:!public! !
!Temporario categoriesFor: #verCont!public! !

!Temporario class methodsFor!

crear:unNom con:ch con:cantH con:unaLic con:unEst con:unCont
^super new init:unNom con: ch con:cantH con:unaLic con:unEst con: unCont! !
!Temporario class categoriesFor: #crear:con:con:con:con:con:!public! !

