"Filed out from Dolphin Smalltalk 7"!

Expediente subclass: #Penal
	instanceVariableNames: 'acusado acusacion pruebasPresentadas testimonios'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Penal guid: (GUID fromString: '{F684530E-9650-48B0-B4FB-E80318AA50F6}')!
Penal comment: ''!
!Penal categoriesForClass!Kernel-Objects! !
!Penal methodsFor!

iniciar: unNumero con: unTitular con: unaFechaP con: unAcusado con: unAcusacion con: unPruebasPresentadas con: unTestimonios
	"Inicializa la clase ExpedientePenal"
	acusado := unAcusado.
	acusacion := unAcusacion.
	pruebasPresentadas := unPruebasPresentadas.
	testimonios := unTestimonios.
	^super iniciar: unNumero con: unTitular con: unaFechaP.!

modAcusacion:unAcusacion
"Modifica acusacion por unAcusacion"
acusacion:=unAcusacion.!

modAcusado:unAcusado
"Modifica acusado por unAcusado"
acusado:=unAcusado.!

modPruebasPresentadas:unPruebasPresentadas
"Modifica pruebasPresentadas por unPruebasPresentadas"
pruebasPresentadas:=unPruebasPresentadas.!

modTestimonios:unTestimonios
"Modifica testimonios por unTestimonios"
testimonios:=unTestimonios.!

verAcusacion
"Retorna la acusacion"
^acusacion.!

verAcusado
"Retorna el nombre del acusado"
^acusado.!

verPruebasPresentadas
"Retorna las pruebas presentadas"
^pruebasPresentadas.!

verTestimonios
"Retorna los testimonios"
^testimonios.! !
!Penal categoriesFor: #iniciar:con:con:con:con:con:con:!public! !
!Penal categoriesFor: #modAcusacion:!public! !
!Penal categoriesFor: #modAcusado:!public! !
!Penal categoriesFor: #modPruebasPresentadas:!public! !
!Penal categoriesFor: #modTestimonios:!public! !
!Penal categoriesFor: #verAcusacion!public! !
!Penal categoriesFor: #verAcusado!public! !
!Penal categoriesFor: #verPruebasPresentadas!public! !
!Penal categoriesFor: #verTestimonios!public! !

!Penal class methodsFor!

crearPenal:unNumero con:unTitular con:unaFechaP  con:unAcusado con:unAcusacion con:unpruebasPresentadas con:unTestimonios
^(super new)iniciar:unNumero con:unTitular con:unaFechaP  con:unAcusado con:unAcusacion con:unpruebasPresentadas con:unTestimonios.! !
!Penal class categoriesFor: #crearPenal:con:con:con:con:con:con:!public! !

