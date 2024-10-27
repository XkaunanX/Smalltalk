"Filed out from Dolphin Smalltalk 7"!

Expediente subclass: #Civil
	instanceVariableNames: 'tipoDeCaso interviniente'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Civil guid: (GUID fromString: '{903406C2-240A-4989-8D42-46D38C96BC97}')!
Civil comment: ''!
!Civil categoriesForClass!Kernel-Objects! !
!Civil methodsFor!

iniciar:unNumero con:unTitular con:unaFechaP con:unTipoDeCaso con:unInterviniente
"Inicializa la clase ExpedientePenal"
tipoDeCaso:=unTipoDeCaso.
interviniente:=unInterviniente.
^super iniciar:unNumero con:unTitular con:unaFechaP.!

modInterviniente:unInterviniente
"Modifica interviniente por unInterviniente"
interviniente:=unInterviniente.!

modTipoDeCaso:unTipoDeCaso
"Modifica tipoDeCaso por unTipoDeCaso"
tipoDeCaso:=unTipoDeCaso.!

verInterviniente
"Retorna el interviniente"
^interviniente.!

verTipoDeCaso
"Retorna el tipoDeCaso"
^tipoDeCaso.! !
!Civil categoriesFor: #iniciar:con:con:con:con:!public! !
!Civil categoriesFor: #modInterviniente:!public! !
!Civil categoriesFor: #modTipoDeCaso:!public! !
!Civil categoriesFor: #verInterviniente!public! !
!Civil categoriesFor: #verTipoDeCaso!public! !

!Civil class methodsFor!

crearCivil:unNumero con:unTitular con:unaFechaP  con:unTipoDeCaso con:unInterviniente
^(super new) iniciar:unNumero con:unTitular con:unaFechaP con:unTipoDeCaso con:unInterviniente.
!

crearCivil:unNumero con:unTitular con:unaFechaP con:unTipo con:unTipoDeCaso con:unInterviniente
^(super new) iniciar:unNumero con:unTitular con:unaFechaP con:unTipo con:unTipoDeCaso con:unInterviniente.
! !
!Civil class categoriesFor: #crearCivil:con:con:con:con:!public! !
!Civil class categoriesFor: #crearCivil:con:con:con:con:con:!public! !

