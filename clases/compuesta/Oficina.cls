"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Oficina
	instanceVariableNames: 'nombre expedientes'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Oficina guid: (GUID fromString: '{51EB851F-373D-4898-AB7B-6C9D1B89AC6B}')!
Oficina comment: ''!
!Oficina categoriesForClass!Kernel-Objects! !
!Oficina methodsFor!

agregarExp:unExpediente
expedientes add: unExpediente.!

eliminar:unExp
expedientes remove:unExp ifAbsent:[^nil ].!

iniciarOficina:unNombre
nombre:=unNombre.
expedientes:=OrderedCollection new.!

modNombre:unNombre
nombre:=unNombre.!

recuperar:unaPos
^expedientes at:unaPos.!

verExpedientes
^expedientes.!

verNombre
^nombre.
!

verTotalExpedientes
^expedientes size.! !
!Oficina categoriesFor: #agregarExp:!public! !
!Oficina categoriesFor: #eliminar:!public! !
!Oficina categoriesFor: #iniciarOficina:!public! !
!Oficina categoriesFor: #modNombre:!public! !
!Oficina categoriesFor: #recuperar:!public! !
!Oficina categoriesFor: #verExpedientes!public! !
!Oficina categoriesFor: #verNombre!public! !
!Oficina categoriesFor: #verTotalExpedientes!public! !

!Oficina class methodsFor!

crearOficina:unNombre
^self new iniciarOficina:unNombre.! !
!Oficina class categoriesFor: #crearOficina:!public! !

