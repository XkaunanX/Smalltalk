"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Expediente
	instanceVariableNames: 'numero titular fechaPresentacion tipo'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Expediente guid: (GUID fromString: '{0E5D2820-E04C-4A4A-8935-3D4477FC3AD1}')!
Expediente comment: ''!
!Expediente categoriesForClass!Kernel-Objects! !
!Expediente methodsFor!

iniciar:unNumero con:unTitular con:unaFechaP 
numero:=unNumero.
titular:=unTitular.
fechaPresentacion:=unaFechaP.
!

modFechaP:unaFechaP
fechaPresentacion:=unaFechaP.!

modNumero:unNumero
numero:=unNumero.!

modTitular:unTitular
titular:=unTitular.!

verFechaP
^fechaPresentacion.!

verNumero
^numero.!

verTitular
^titular.! !
!Expediente categoriesFor: #iniciar:con:con:!public! !
!Expediente categoriesFor: #modFechaP:!public! !
!Expediente categoriesFor: #modNumero:!public! !
!Expediente categoriesFor: #modTitular:!public! !
!Expediente categoriesFor: #verFechaP!public! !
!Expediente categoriesFor: #verNumero!public! !
!Expediente categoriesFor: #verTitular!public! !

!Expediente class methodsFor!

crearExpediente:unNumero con:unTitular con:unaFechaP 
^(super new) iniciar:unNumero con:unTitular con:unaFechaP.!

crearExpediente:unNumero con:unTitular con:unaFechaP con:unTipo
^(super new) iniciar:unNumero con:unTitular con:unaFechaP con:unTipo.! !
!Expediente class categoriesFor: #crearExpediente:con:con:!public! !
!Expediente class categoriesFor: #crearExpediente:con:con:con:!public! !

