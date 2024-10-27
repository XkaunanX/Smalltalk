"Filed out from Dolphin Smalltalk 7"!

Object subclass: #Remedio
	instanceVariableNames: 'nombre precio stock laboratorio'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!
Remedio guid: (GUID fromString: '{C6F6D3EA-BC19-4DB2-8A52-22C58961239D}')!
Remedio comment: ''!
!Remedio categoriesForClass!Kernel-Objects! !
!Remedio methodsFor!

initiate:alias precio:unPrecio stock:unStock laboratorio:unLaboratorio
"El metodo de clase asigna los valores de las variables de instancia"
"No pueden tener el mismo nombre ej: nombre:=nombre osea que no se puede repetir el nombre de la variable de instancia con el parametro que reciba el metodo"
"Operaciones de clase comun
Mod
Ver
"
nombre:=alias.
precio:=unPrecio.
stock:=unStock.
laboratorio:=unLaboratorio.!

modLaboratorio: unLaboratorio
"modificar laboratorio"
laboratorio:=unLaboratorio.!

modNombre: unAlias
"modifico el alias del objeto apuntado"
nombre:=unAlias.!

modPrecio: unPrecio
"Modifica el precio"
precio:=unPrecio.!

modStock: unStock
"modifico el stock del objeto apuntado"
stock:=unStock.!

verLaboratorio
"devuelvo el dato de la variable de instancia ver laboratorio de la clase remedio"
^laboratorio!

verNombre
"Devuelvo el nombre del objeto apuntado"
^nombre!

verPrecio
"devuelvo el precio del objeto apuntado"
^precio!

verStock
"devulevo el stock del objeto apuntado"
^stock! !
!Remedio categoriesFor: #initiate:precio:stock:laboratorio:!public! !
!Remedio categoriesFor: #modLaboratorio:!public! !
!Remedio categoriesFor: #modNombre:!public! !
!Remedio categoriesFor: #modPrecio:!public! !
!Remedio categoriesFor: #modStock:!public! !
!Remedio categoriesFor: #verLaboratorio!public! !
!Remedio categoriesFor: #verNombre!public! !
!Remedio categoriesFor: #verPrecio!public! !
!Remedio categoriesFor: #verStock!public! !

!Remedio class methodsFor!

new:alias precio:unPrecio stock:unStock laboratorio:unLaboratorio
"Metodo de clase que crea una instancia de la clase remedio"
"A mi mismo construyo e instancio con un alias precio stock laboratorio"
^self new initiate:alias precio:unPrecio stock:unStock laboratorio:unLaboratorio! !
!Remedio class categoriesFor: #new:precio:stock:laboratorio:!public! !

