"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Empleado
	instanceVariableNames: 'nombre horas hijos licencia estCivil'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Empleado guid: (GUID fromString: '{F56D7916-9875-4673-8F99-8DE4D8F198F5}')!
Empleado comment: ''!
!Empleado categoriesForClass!Kernel-Objects! !
!Empleado methodsFor!

basico 
"Retorna el sueldo basico del empleado (método abstracto)"
^self subclassResponsibility!

init:unNom con:cantHr con:cantHi con:unaLic con:unEst
"Inicializa la clase empleado"
nombre:=unNom.
horas:=cantHr.
hijos:=cantHi.
estCivil:=unEst.
licencia:=unaLic.!

modCantH:unaCant
"Modifica la cantidad de horas del empleado por unaCant"
horas:=unaCant.!

modEstCi:unEst
"Modifica el estado civil del empleado por unEst"
estCivil:=unEst.!

modHijos:unaCantH
"Modifica la cantidad de hijos del empleado por unaCantH"
hijos:=unaCantH.
!

modLic: unaLic
"Modifica la licencia del empleado por unaLic"
licencia:=unaLic.!

modNom:unNom
"Modifica el nombre del empleado por unNom"
nombre:=unNom.!

salario 
"Retorna el salario familiar del empleado"
|s|
s:= 200*hijos.
(estCivil = 'Casado')ifTrue:[s:=s+1000].
^s.!

sueldo
"Retorna el sueldo total del empleado"
^(self basico + self salario)!

verCantH
"Retorna la cantidad de horas del empleado"
^horas.!

verEstCi 
"Retorna el estado civil del elemento del empleado"
^estCivil.!

verHijos
"Retorna la cantidad de hijos del empleado"
^hijos.!

verLic 
"Retorna la licencia del empleado"
^licencia.!

verNom
"Retorna el nombre del empleado"
^nombre.! !
!Empleado categoriesFor: #basico!public! !
!Empleado categoriesFor: #init:con:con:con:con:!public! !
!Empleado categoriesFor: #modCantH:!public! !
!Empleado categoriesFor: #modEstCi:!public! !
!Empleado categoriesFor: #modHijos:!public! !
!Empleado categoriesFor: #modLic:!public! !
!Empleado categoriesFor: #modNom:!public! !
!Empleado categoriesFor: #salario!public! !
!Empleado categoriesFor: #sueldo!public! !
!Empleado categoriesFor: #verCantH!public! !
!Empleado categoriesFor: #verEstCi!public! !
!Empleado categoriesFor: #verHijos!public! !
!Empleado categoriesFor: #verLic!public! !
!Empleado categoriesFor: #verNom!public! !

!Empleado class methodsFor!

crearEmp:unNom con:cantHr con:cantHi con:unaLic con:unEst
"Crea la clase Empleado"
^(super new) init:unNom con:cantHr con:cantHi con:unaLic con:unEst.
! !
!Empleado class categoriesFor: #crearEmp:con:con:con:con:!public! !

