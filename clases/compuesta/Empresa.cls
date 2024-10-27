"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Empresa
	instanceVariableNames: 'nombre empleados'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Empresa guid: (GUID fromString: '{685ADCD4-A43C-4BC9-865D-2B9A0F61C487}')!
Empresa comment: ''!
!Empresa categoriesForClass!Kernel-Objects! !
!Empresa methodsFor!

agregarEmpleado:emp
"Agrega un nuevo empleado emp a la Empresa"
empleados add:emp.!

eliminarEmpleado:emp
"Elimina un empleado emp de la Empresa"
empleados remove:emp.!

esVacia
"Valida si hay empleados en la empresa"
^empleados isEmpty.!

existeEmpleado:emp
"Valida si está el empleado en la empresa"
empleados includes:emp.!

initEmpresa:unNom
"Inicializa la clase Empresa"
nombre:=unNom.
empleados:=OrderedCollection new.
!

modNombre:unNom
"Modifica el nombre de la empresa por unNom"
nombre:=unNom.!

recuperarEmpleado: i
"Retorna el i-esimo empleado"
empleados at:i.!

verEmpleados
"Retorna todos los empleados de la Empresa"
^empleados!

verNombre
^nombre.!

verTotalEmpleados
"Retorna el total de empleados que tiene la Empresa"
^empleados size.! !
!Empresa categoriesFor: #agregarEmpleado:!public! !
!Empresa categoriesFor: #eliminarEmpleado:!public! !
!Empresa categoriesFor: #esVacia!public! !
!Empresa categoriesFor: #existeEmpleado:!public! !
!Empresa categoriesFor: #initEmpresa:!public! !
!Empresa categoriesFor: #modNombre:!public! !
!Empresa categoriesFor: #recuperarEmpleado:!public! !
!Empresa categoriesFor: #verEmpleados!public! !
!Empresa categoriesFor: #verNombre!public! !
!Empresa categoriesFor: #verTotalEmpleados!public! !

!Empresa class methodsFor!

crearEmpresa: unNom
"Crea la clase empresa"
^(super new) initEmpresa:unNom! !
!Empresa class categoriesFor: #crearEmpresa:!public! !

