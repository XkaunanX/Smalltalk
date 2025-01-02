"Filed out from Dolphin Smalltalk 7"!

Consulta subclass: #Regular
	instanceVariableNames: 'diagnostico receta'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Regular guid: (GUID fromString: '{D595A343-8EC8-4BFC-9437-64BF5515FEE7}')!
Regular comment: ''!
!Regular categoriesForClass!Kernel-Objects! !
!Regular methodsFor!

init:num con:med con:fec con:hor con:tip con:unaedad con:undni con:unnombre con:diag con:rec con:unsintoma
"Inicializa una consulta regular"
super init: num con: med con: fec con: hor con: tip con:unaedad con:undni con:unnombre con:unsintoma.

diagnostico:=diag.
receta:=rec.!

modDiagnostico:otroDiag "Modifica el diagnóstico de la consulta"
diagnostico:=otroDiag.!

modReceta:otraReceta "Modifica la receta de la consulta"
receta:=otraReceta.!

verDiagnostico "Retorna el diagnóstico de la consulta"
^diagnostico!

verReceta "Retorna la receta de la consulta"
^receta! !
!Regular categoriesFor: #init:con:con:con:con:con:con:con:con:con:con:!public! !
!Regular categoriesFor: #modDiagnostico:!public! !
!Regular categoriesFor: #modReceta:!public! !
!Regular categoriesFor: #verDiagnostico!public! !
!Regular categoriesFor: #verReceta!public! !

!Regular class methodsFor!

crear:num con:med con:fec con: hor con: tip con:unaedad con:undni con:unnombre con:unsintoma
"Crea una instancia de la clase Consulta y la inicializa con sus datos"
^self new init:num con:med con:fec con:hor con:tip con:unaedad con:unnombre con:undni con:unsintoma! !
!Regular class categoriesFor: #crear:con:con:con:con:con:con:con:con:!public! !

