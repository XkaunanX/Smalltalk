"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Consulta
	instanceVariableNames: 'numero medico fecha hora tipo dni nombre edad sintoma nombreMedico'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Consulta guid: (GUID fromString: '{D4FD2181-2F0C-4B35-B26D-CE942F6418FA}')!
Consulta comment: ''!
!Consulta categoriesForClass!Kernel-Objects! !
!Consulta methodsFor!

init:num con:med con:fec con:hor con:tip con:unaedad con:undni con:unnombre con:unsintoma
"Inicializa una consulta con sus datos"
numero:=num.
medico:=med.
fecha:=fec.
hora:=hor.
tipo:=tip.
edad:=unaedad.
dni:=undni.
nombre:=unnombre.
sintoma:=unsintoma!

modDni: otroDni "Modifica el dni"

dni:=otroDni.!

modEdad:otraEdad
edad:=otraEdad.!

modFecha:otraFecha "Modifica la fecha de consulta"
fecha:=otraFecha.!

modHora:otraHora "Modifica la hora de la consulta"
hora:=otraHora.!

modMed: otroMed "Modifica  el médico asignado a la consulta"
medico:=otroMed.!

modNombre:otroNombre "Modifica el tipo de consulta"
nombre :=otroNombre.!

modNum: otroNum "Modifica el número de consulta"
numero:=otroNum.!

modSintoma:otroSintomas
sintoma:=otroSintomas.!

modTipo:otroTipo "Modifica el tipo de consulta"
tipo:=otroTipo.!

verDni "Retorna el dni"
^dni!

verEdad "Retorna la edad del paciente"
^edad!

verFecha "Retorna la fecha de la consulta"
^fecha!

verHora "Retorna la hora de la consulta"
^hora!

verMed "Retorna el médico asignado"
^medico!

verNombre "Retorna el nombre del paciente"
^nombre!

verNum "Retorna el número de consulta"
^numero!

verSintoma
^sintoma!

verTipo "Retorna el tipo de consulta"
^tipo! !
!Consulta categoriesFor: #init:con:con:con:con:con:con:con:con:!public! !
!Consulta categoriesFor: #modDni:!public! !
!Consulta categoriesFor: #modEdad:!public! !
!Consulta categoriesFor: #modFecha:!public! !
!Consulta categoriesFor: #modHora:!public! !
!Consulta categoriesFor: #modMed:!public! !
!Consulta categoriesFor: #modNombre:!public! !
!Consulta categoriesFor: #modNum:!public! !
!Consulta categoriesFor: #modSintoma:!public! !
!Consulta categoriesFor: #modTipo:!public! !
!Consulta categoriesFor: #verDni!public! !
!Consulta categoriesFor: #verEdad!public! !
!Consulta categoriesFor: #verFecha!public! !
!Consulta categoriesFor: #verHora!public! !
!Consulta categoriesFor: #verMed!public! !
!Consulta categoriesFor: #verNombre!public! !
!Consulta categoriesFor: #verNum!public! !
!Consulta categoriesFor: #verSintoma!public! !
!Consulta categoriesFor: #verTipo!public! !

!Consulta class methodsFor!

crear:num con:med con:fec con: hor con: tip con:unaedad con:undni con:unnombre con:unsintoma
"Crea una instancia de la clase Consulta y la inicializa con sus datos"
^self new init:num con:med con:fec con:hor con:tip con:unaedad con:unnombre con:undni con:unsintoma! !
!Consulta class categoriesFor: #crear:con:con:con:con:con:con:con:con:!public! !

