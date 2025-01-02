"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Consultorio
	instanceVariableNames: 'cantidadPacientes especialidad nombre numeroMedicos consultas pacientesConsultas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Consultorio guid: (GUID fromString: '{C0273DE7-C629-40D9-8345-D2F5CC284F91}')!
Consultorio comment: ''!
!Consultorio categoriesForClass!Kernel-Objects! !
!Consultorio methodsFor!

agregar: unaConsulta

consultas add:unaConsulta.
!

eliminar: unaConsulta

consultas remove: unaConsulta ifAbsent:[^nil].
!

esVacio

^consultas isEmpty.
!

existe: unaConsulta

^consultas includes: unaConsulta.!

iniciar: unaCantPacien especialidad:unaEspecialidad nombre:unNombre numeroMedicos:unNumeroMed

"Inicializa el consultorio con sus datos"

cantidadPacientes:=unaCantPacien.
especialidad:=unaEspecialidad.
nombre:=unNombre.
numeroMedicos:=unNumeroMed.
consultas:=OrderedCollection new.
pacientesConsultas:=Dictionary new.

!

modCantidadPacientes:otraCantidad

cantidadPacientes:=otraCantidad.!

modEspecialidad:otraEspecialidad

especialidad:=otraEspecialidad.!

modNombre:otroNombre

nombre:=otroNombre.
!

modNumeroMedicos:otroNumeroMedicos

numeroMedicos:=otroNumeroMedicos.!

recuperar: i

^consultas at: i.
!

tamanio

^consultas size.!

verCantidadPacientes

^cantidadPacientes!

verEspecialidad

^especialidad!

verNombre

^nombre!

verNumeroMedicos

^numeroMedicos!

verTodos

^consultas.! !
!Consultorio categoriesFor: #agregar:!public! !
!Consultorio categoriesFor: #eliminar:!public! !
!Consultorio categoriesFor: #esVacio!public! !
!Consultorio categoriesFor: #existe:!public! !
!Consultorio categoriesFor: #iniciar:especialidad:nombre:numeroMedicos:!public! !
!Consultorio categoriesFor: #modCantidadPacientes:!public! !
!Consultorio categoriesFor: #modEspecialidad:!public! !
!Consultorio categoriesFor: #modNombre:!public! !
!Consultorio categoriesFor: #modNumeroMedicos:!public! !
!Consultorio categoriesFor: #recuperar:!public! !
!Consultorio categoriesFor: #tamanio!public! !
!Consultorio categoriesFor: #verCantidadPacientes!public! !
!Consultorio categoriesFor: #verEspecialidad!public! !
!Consultorio categoriesFor: #verNombre!public! !
!Consultorio categoriesFor: #verNumeroMedicos!public! !
!Consultorio categoriesFor: #verTodos!public! !

!Consultorio class methodsFor!

crear: unaCantPacien con: unaEspecialidad con: unNombre con:unNumMed

"Crea una instancia de la clase Consultorio y la inicializa con sus datos"

^self new iniciar: unaCantPacien especialidad: unaEspecialidad nombre: unNombre numeroMedicos: unNumMed! !
!Consultorio class categoriesFor: #crear:con:con:con:!public! !

