"Filed out from Dolphin Smalltalk 7"!

Consulta subclass: #Urgente
	instanceVariableNames: 'prioridad atencion'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Urgente guid: (GUID fromString: '{1AB7FE7F-A5AE-405E-95A0-D5EA6FC394B6}')!
Urgente comment: ''!
!Urgente categoriesForClass!Kernel-Objects! !
!Urgente methodsFor!

init:num con:med con:fec con:hor con:tip con:unaedad con:undni con:unnombre con:prior con:aten con:unsintoma
"Inicializa una consulta regular"
super init: num con: med con: fec con: hor con: tip con:unaedad con:undni con:unnombre con:unsintoma.
prioridad := prior.
atencion := aten.!

modAtencion:otraAtencion "Modifica el tiempo de atención de la consulta urgente"
atencion:=otraAtencion.!

modPrioridad:otraPrioridad "Modifica la prioridad de la consulta urgente"
prioridad:=otraPrioridad.!

verAtencion "Muestra el tiempo de atención de la consulta de urgencia"
^atencion!

verPrioridad "Muestra la prioridad de la consulta urgente"
^prioridad! !
!Urgente categoriesFor: #init:con:con:con:con:con:con:con:con:con:con:!public! !
!Urgente categoriesFor: #modAtencion:!public! !
!Urgente categoriesFor: #modPrioridad:!public! !
!Urgente categoriesFor: #verAtencion!public! !
!Urgente categoriesFor: #verPrioridad!public! !

!Urgente class methodsFor!

crear:num con:med con:fec con: hor con: tip con:unaedad con:undni con:unnombre con:prior con:aten con:unsintoma
"Crea una instancia de la clase Consulta y la inicializa con sus datos"
^self new init:num con:med con:fec con:hor con:tip con:unaedad con:undni con:unnombre con:prior con:aten con:unsintoma.

"init:num con:med con:fec con:hor con:tip con:unaedad con:undni con:unnombre con:prior con:aten con:unsintoma"! !
!Urgente class categoriesFor: #crear:con:con:con:con:con:con:con:con:con:con:!public! !

