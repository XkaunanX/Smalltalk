| pacien espec nombre medicos consultorio tipoConsulta  dni edad fecha hora medico  id sintoma consulta  minutosValidos minutos  horas   ejecucion receta diagnostico  sumatoriaEdad cont menuRegular menuUrgente|

id := 0.

sumatoriaEdad:=0.

cont:=0.

"variable que indica ejecucion"
ejecucion:=true.


menuOption:=#('Agregar consulta' 'Modificar consulta' 'Eliminar consulta' 'Listar consultas de un dia especifico' 'Listar consultas urgentes' 'Informe edad promedio de pacientes por franja horaria' 'Listar consultas reservadas sin contar vacaciones del medico' 'Generar diccionario de cantidad de consultas por paciente'  'Listar consultas' 'Salir').
menuRegular:=#('DNI del paciente' 'Edad' 'Fecha y/u hora' 'Médico asignado' 'Nombre completo' 'Número de consulta' 'Síntomas' 'Receta' 'Diagnóstico').
menuUrgente:=#('DNI del paciente' 'Edad' 'Fecha y/u hora' 'Médico asignado' 'Nombre completo' 'Número de consulta' 'Síntomas' 'Prioridad' 'Tiempo de atención').
menuTipo:=#('Urgente' 'Regular').

"Menu atributo de consultas comunes"
menuComun:=#().

menuFranja:=#('Manana' 'Tarde' 'Noche').

minutosValidos := #('00' '30').

"Crear consultorio"
	pacien := (Prompter prompt: 'Ingrese la cantidad de pacientes') asNumber.
	espec := (Prompter prompt: 'Ingrese la cantidad de especialidades que ofrece') asNumber.
	nombre := Prompter prompt: 'Ingrese el nombre del consultorio'.
	medicos := (Prompter prompt: 'Ingrese la cantidad de medicos disponibles') asNumber.

"instanciar consultorio"
	consultorio := Consultorio crear: pacien con: espec con: nombre con: medicos.

	consultas := consultorio verTodos.


"MainLoop"
	[ ejecucion ] whileTrue: [
		"Menu"
		opcion := ChoicePrompter choices: menuOption caption: consultorio verNombre.
		"Bifurcaciones"
		(opcion = 'Agregar consulta' ) ifTrue: [ 
		"Ingreso de datos comunes"
		id := (Prompter prompt: 'Ingrese numero de consulta') asNumber.
		dni := (Prompter prompt: 'Ingrese el DNI del paciente') asNumber.
		edad := (Prompter prompt: 'Ingrese la edad del paciente') asNumber.
		fecha:=Date fromString: (Prompter prompt: 'Ingrese la fecha de la consulta (DD/MM/AAAA)').
		 "Validación de minutos (como se dan turnos cada media hora solo se asignan en punto o y media)"
		 minutosValidos := #('00' '30').
		 [hora := Prompter prompt: 'Ingrese la hora HH:MM'.
		 hora notNil and: [
		 DosPuntos := hora findLast: [:ch | ch = $:].
			minutos := hora copyFrom: DosPuntos + 1 to: hora size.
			(minutosValidos includes: minutos) not]] whileTrue: [
			    MessageBox warning: 'Debe ingresar una hora con minutos en 00 o 30'.
		].
	    hora := Time fromString: hora.
	    medico := Prompter prompt: 'Ingrese nombre del medico'.
	    nombre := Prompter prompt: 'Ingrese el nombre del paciente'.
	    sintoma := Prompter prompt: 'Ingrese el sintoma motivo de la consulta'.
	tipoConsulta := Prompter prompt: '¿Es consulta regular o urgente? (r/u)'.	
			(tipoConsulta = 'u') ifTrue:[
				prioridad := (Prompter prompt: 'Ingrese la prioridad 1=BAJA 2=MEDIA 3=ALTA ') asNumber.
				atencion := Prompter prompt: 'Ingrese el tiempo de atencion estimado en minutos'.
				consulta := Urgente crear: id con: medico con: fecha con: hora con: tipoConsulta con: edad con: dni con: nombre con: prioridad con: atencion con: sintoma.
				
				regulares := consultas select: [:item | item isKindOf: Regular ].
				regularesFecha := regulares select: [:item | item verFecha =  fecha].
				superpuestas := regularesFecha select: [:item | item verHora >= hora].
				
				Transcript clear.
				Transcript show: consulta verNombre.

				consultorio agregar: consulta.

				(superpuestas isEmpty) ifFalse: [
					proxima := hora copy.
					superpuestas do: [:item | 
						horas := proxima hours.
						minutos := proxima minutes + 30.
						(minutos >= 60) ifTrue: [
							horas := horas + 1.
							minutos := minutos - 60.
						].
						nueva_hora := Time hours: horas minutes: minutos seconds: 0.
						item modHora: nueva_hora.
						proxima := nueva_hora.
					].
					MessageBox warning: 'Consulta urgente agregada y horarios de regulares desplazados'.
				] ifTrue:[
					MessageBox notify: 'Consulta urgente cargada'.

				].		
				"se considera el tiempo de atencion posterior a la atencion, por eso no se carga aca, puede agregarse desde modificar consulta"
				
			].
			(tipoConsulta = 'r') ifTrue:[
				consultas := consultorio verTodos.
				"Filtro por medico"
				medi := consultas select: [:item | item verMed = medico].
				"Filtrar por fecha"
				fech := medi select: [:item | item verFecha = fecha].
				"Filtrar por hora"
				hor := fech select: [:item | item verHora = hora].
				(hor isEmpty ) ifFalse: [
					MessageBox errorMsg: 'Ya hay una consulta con ese medico ese dia y a esa hora'.
				] ifTrue: [
				"Ingresar datos regular"
				receta:=0.
				diagnostico:=0.
				consulta := Regular new init: id con: medico con: fecha con: hora con: tipoConsulta con: edad con: dni con: nombre con: diagnostico con: receta con: sintoma.
				consultorio agregar: consulta.
				"se considera el diagnostico y receta  posterior a la atencion, por eso no se carga aca, puede agregarse desde modificar consulta"
				 ].
		
		].

	].

	(opcion = 'Modificar consulta') ifTrue: [
	    "Recuperar todas las consultas del consultorio"
	    consultas := consultorio verTodos.
	    "Ingresar id"
	    input := (Prompter prompt: 'Ingrese número de consulta') asNumber.
	    "Transcript show: input; cr. 

	    id := input asNumber ifNil: [
		MessageBox box: 'Por favor ingrese un número válido.'.
		^self.
	    ]."
	    "Recuperar con detect"
	    modificado := consultas detect: [:item | item verNum == input ] ifNone: [
		MessageBox notify: 'No se ha encontrado la consulta'.
		^self.
	    ].

	    "Si encontró la consulta"
	    (modificado isKindOf: Regular) ifTrue: [
		var := ChoicePrompter choices: menuRegular.
		
		"Modificación del DNI del paciente"
		(var = 'DNI del paciente') ifTrue: [
		    dniNuevo := (Prompter prompt: 'Ingrese el nuevo DNI') asNumber.
		    modificado modDni: dniNuevo.
		    MessageBox notify: 'Consulta encontrada y modificada'.
		].

		"Modificación de la Edad"
		(var = 'Edad') ifTrue: [
		    edadNueva := (Prompter prompt: 'Ingrese la nueva edad') asNumber.
		    modificado modEdad: edadNueva.
		    MessageBox notify: 'Consulta encontrada y edad modificada'.
		].

		"Modificación de la Fecha o Hora"
		(var = 'Fecha y/u hora') ifTrue: [
		    
		    consulta := consultas detect: [:item | item verNum = input] ifNone: [MessageBox inform: 'No se encontró la consulta con ese ID.'].

		    opcion := ChoicePrompter choices: #('Modificar hora' 'Modificar fecha') caption: 'Seleccione una opción'.

		    (opcion = 'Modificar hora') ifTrue: [
			
			nuevaHora := Time fromString: (Prompter prompt: 'Ingrese la nueva hora de la consulta (HH:MM)') asString.
			
			consultaExistente := consultas detect: [:item | (item verFecha = consulta verFecha and: [item verHora = nuevaHora])] ifNone: [nil].

			consultaExistente ifNotNil: [
			    MessageBox errorMsg: 'Ya existe una consulta en esa fecha y hora. No se puede modificar el horario.'.
			    ^self. 
			].

			consulta modHora: nuevaHora.
			MessageBox notify: 'La hora de la consulta ha sido modificada exitosamente.'.
		    ].

		    (opcion = 'Modificar fecha') ifTrue: [
			nuevaFecha := Date fromString: (Prompter prompt: 'Ingrese la nueva fecha de la consulta (DD/MM/AAAA)') asString.

			
			consultaExistente := consultas detect: [:item | (item verFecha = nuevaFecha and: [item verHora = consulta verHora])] ifNone: [nil].

			consultaExistente ifNotNil: [
			    MessageBox errorMsg: 'Ya existe una consulta en esa fecha y hora. No se puede modificar la fecha.'.
			    ^self. 
			].

			consulta modFecha: nuevaFecha.
			MessageBox notify: 'La fecha de la consulta ha sido modificada exitosamente.'.
		    ].
		].

		"Modificación del Médico asignado"
		(var = 'Médico asignado') ifTrue: [
		    nuevoMedico := Prompter prompt: 'Ingrese el nuevo médico'.
		    modificado modMed: nuevoMedico.
		    MessageBox notify: 'Consulta encontrada y médico asignado modificado'.
		].

		"Modificación del Nombre completo"
		(var = 'Nombre completo') ifTrue: [
		    nuevoNombre := Prompter prompt: 'Ingrese el nuevo nombre'.
		    modificado modNombre: nuevoNombre.
		    MessageBox notify: 'Consulta encontrada y nombre completo modificado'.
		].

		"Modificación del Número de consulta"
		(var = 'Número de consulta') ifTrue: [
		    nuevoNumero := (Prompter prompt: 'Ingrese el nuevo número de consulta') asNumber.
		    modificado modNum: nuevoNumero.
		    MessageBox notify: 'Consulta encontrada y número de consulta modificado'.
		].

		"Modificación de los Síntomas"
		(var = 'Síntomas') ifTrue: [
		    nuevoSintoma := Prompter prompt: 'Ingrese los nuevos síntomas'.
		    modificado modSintoma: nuevoSintoma.
		    MessageBox notify: 'Consulta encontrada y síntoma modificado'.
		].
	
		(var = 'Diagnóstico') ifTrue: [
		    nuevoDiagnostico := Prompter prompt: 'Ingrese el nuevo diagnóstico'.
		    modificado modDiagnostico: nuevoDiagnostico.
		    MessageBox notify: 'Consulta encontrada y diagnóstico modificado'.
		].

		(var = 'Receta') ifTrue: [
		    nuevaReceta := Prompter prompt: 'Ingrese la nueva receta'.
		    modificado modReceta: nuevaReceta.
		    MessageBox notify: 'Consulta encontrada y receta modificada'.
		].

	    ].

	    "Si la consulta es urgente"
	    (modificado isKindOf: Urgente) ifTrue: [
		var := ChoicePrompter choices: menuUrgente.

		"Modificación del DNI del paciente"
		(var = 'DNI del paciente') ifTrue: [
		    dniNuevo := (Prompter prompt: 'Ingrese el nuevo DNI') asNumber.
		    modificado modDni: dniNuevo.
		    MessageBox notify: 'Consulta encontrada y modificada'.
		].

		"Modificación de la Edad"
		(var = 'Edad') ifTrue: [
		    edadNueva := (Prompter prompt: 'Ingrese la nueva edad') asNumber.
		    modificado modEdad: edadNueva.
		    MessageBox notify: 'Consulta encontrada y edad modificada'.
		].

		"Modificación de la Fecha o Hora"
		(var = 'Fecha y/u hora') ifTrue: [

		    consulta := consultas detect: [:item | item verNum = id] ifNone: [MessageBox inform: 'No se encontró la consulta con ese ID.'].

		    opcion := ChoicePrompter choices: #('Modificar hora' 'Modificar fecha') caption: 'Seleccione una opción'.

		    (opcion = 'Modificar hora') ifTrue: [

			nuevaHora := Time fromString: (Prompter prompt: 'Ingrese la nueva hora de la consulta (HH:MM)') asString.

			"Me fijo que no haya una consulta en ese día y horario"
			consultaExistente := consultas detect: [:item | (item verFecha = consulta verFecha and: [item verHora = nuevaHora])] ifNone: [nil].

			consultaExistente ifNotNil: [
			    MessageBox errorMsg: 'No se puede modificar el horario, ya hay una consulta en ese horario'.
			    ^self. 
			].

			consulta modHora: nuevaHora.
			MessageBox notify: 'La hora de la consulta ha sido modificada exitosamente.'.
		    ].

		    (opcion = 'Modificar fecha') ifTrue: [

			nuevaFecha := Date fromString: (Prompter prompt: 'Ingrese la nueva fecha de la consulta (DD/MM/AAAA)') asString.

			consultaExistente := consultas detect: [:item | (item verFecha = nuevaFecha and: [item verHora = consulta verHora])] ifNone: [nil].

			consultaExistente ifNotNil: [
			    MessageBox errorMsg: 'Ya existe una consulta en esa fecha y hora. No se puede modificar la fecha.'.
			    ^self. 
			].
			consulta modFecha: nuevaFecha.
			MessageBox notify: 'La fecha de la consulta ha sido modificada exitosamente.'.
		    ].
		].

		"Modificación del Médico asignado"
		(var = 'Médico asignado') ifTrue: [ "No anda"
		    nuevoMedico := Prompter prompt: 'Ingrese el nuevo médico'.
		    modificado modMed: nuevoMedico.
		    MessageBox notify: 'Consulta encontrada y médico asignado modificado'.
		].

		"Modificación del Nombre completo"
		(var = 'Nombre completo') ifTrue: [
		    nuevoNombre := Prompter prompt: 'Ingrese el nuevo nombre'.
		    modificado modNombre: nuevoNombre.
		    MessageBox notify: 'Consulta encontrada y nombre completo modificado'.
		].

		"Modificación del Número de consulta"
		(var = 'Número de consulta') ifTrue: [
		    nuevoNumero := (Prompter prompt: 'Ingrese el nuevo número de consulta') asNumber.
		    modificado modNum: nuevoNumero.
		    MessageBox notify: 'Consulta encontrada y número de consulta modificado'.
		].

		"Modificación de los Síntomas"
		(var = 'Síntomas') ifTrue: [
		    nuevoSintoma := Prompter prompt: 'Ingrese los nuevos síntomas'.
		    modificado modSintoma: nuevoSintoma.
		    MessageBox notify: 'Consulta encontrada y síntoma modificado'.
		].

		(var = 'Prioridad') ifTrue: [
		    nuevaPrioridad := Prompter prompt: 'Ingrese la nueva prioridad'.
		    modificado modPrioridad: nuevaPrioridad.
		    MessageBox notify: 'Consulta encontrada y prioridad modificada'.
		].

		(var = 'Tiempo de atención') ifTrue: [
		    nuevoTiempoAtencion := Prompter prompt: 'Ingrese el nuevo tiempo de atención'.
		    modificado modAtencion: nuevoTiempoAtencion .
		    MessageBox notify: 'Consulta encontrada y tiempo de atención modificado'.
		].
		
	    ].
	].

		    
	(opcion = 'Eliminar consulta') ifTrue: [ 
		
		consultas := consultorio verTodos.
		
		input := (Prompter prompt: 'Ingrese número de consulta') asNumber.
		
		eliminado := consultas detect: [:item | item verNum = input].

		(eliminado isNil) ifTrue: [
		    MessageBox errorMsg: 'No encontrado'.
		] ifFalse: [
		    consultorio eliminar: eliminado.
		    MessageBox notify: 'Consulta eliminada' .
		    
		    "Listar las consultas restantes para verificar la eliminación"
		Transcript clear.
			 consultas do: [:c2 |
            Transcript show: 
            'Consulta N°', c2 verNum printString; cr;
            show:' | Tipo: ', c2 verTipo displayString ; cr;
            show:' | Paciente: ', c2 verNombre printString , ' '; cr;
            show:' | Médico: ', c2 verMed printString ; cr;
            show:' | Fecha: ', c2 verFecha printString; cr;
            show:' | Hora: ', c2 verHora printString;
            cr.
		].
	].
].


	(opcion = 'Listar consultas de un dia especifico' ) ifTrue: [
		"Recuperar todas las consultas del consultorio"
		consultas := consultorio verTodos.
		"Ingresar fecha"
		fechaEspecifica:= Date fromString: (Prompter prompt: 'Ingrese la fecha que desea filtrar (DD/MM/AAAA)') asString.
		filtrados := consultas select: [:each | each verFecha = fechaEspecifica].
		"filtramos las consultas del dia especificado"
		Transcript clear.
		 filtrados do: [:c1 |
            Transcript show: 
            'Consulta N°', c1 verNum printString; cr;
            show:' | Tipo: ', c1 verTipo displayString ; cr;
            show:' | Paciente: ', c1 verNombre printString , ' '; cr;
            show:' | Médico: ', c1 verMed printString ; cr;
            show:' | Fecha: ', c1 verFecha printString; cr;
            show:' | Hora: ', c1 verHora printString;
            cr.
		].
		
	].

	(opcion = 'Listar consultas urgentes' ) ifTrue: [ "Anda"
		consultas := consultorio verTodos.
		urgentes := consultas select: [:solourgentes | solourgentes isKindOf: Urgente].
		
		ordenadas := urgentes asSortedCollection: [:a :b | a verPrioridad > b verPrioridad ].
		Transcript clear.
		ordenadas do: [:urgentesordenadas | 
			  Transcript show: 
			    'Consulta N°', urgentesordenadas verNum printString; cr;
			    show:' | Tipo: ', urgentesordenadas verTipo displayString ; cr;
			    show:' | Paciente: ', urgentesordenadas verNombre printString , ' '; cr;
			    show:' | Médico: ', urgentesordenadas verMed printString ; cr;
			    show:' | Fecha: ', urgentesordenadas verFecha printString; cr;
			    show:' | Hora: ', urgentesordenadas verHora printString;
			    cr.
		].
	].
	
	(opcion = 'Informe edad promedio de pacientes por franja horaria' ) ifTrue: [
	Transcript clear.
	consultas := consultorio verTodos.
	menuEda := ChoicePrompter choices: menuFranja caption: consultorio verNombre.

	(menuEda = 'Manana') ifTrue: [
		manana := consultas select: [:each | each verHora >= (Time hours: 8 minutes: 0 seconds: 0)].
		manana := manana select: [:each | each verHora < (Time hours: 13 minutes: 0 seconds: 0)].
		
		manana do: [:item | 
			sumatoriaEdad := sumatoriaEdad + item verEdad.
			cont := cont + 1.
		]. 
		
		cont = 0 ifTrue: [
			Transcript show: 'No existen consultas en la mañana.'; cr.
		] ifFalse: [
			promedio := sumatoriaEdad / cont.
			Transcript show: 'El promedio de la mañana es: ', promedio displayString; cr.
		].
		
		sumatoriaEdad := 0.
		cont := 0.
	].
	
	(menuEda = 'Tarde') ifTrue: [
		tarde := consultas select: [:each | each verHora >= (Time hours: 14 minutes: 0 seconds: 0)].
		tarde := tarde select: [:each | each verHora < (Time hours: 18 minutes: 0 seconds: 0)].
		
		tarde do: [:item | 
			sumatoriaEdad := sumatoriaEdad + item verEdad.
			cont := cont + 1.
		]. 
		
		cont = 0 ifTrue: [
			Transcript show: 'No existen consultas en la tarde.'; cr.
		] ifFalse: [
			promedio := sumatoriaEdad / cont.
			Transcript show: 'El promedio de la tarde es: ', promedio displayString; cr.
		].
		
		sumatoriaEdad := 0.
		cont := 0.
	].
	
	(menuEda = 'Noche') ifTrue: [
		noche := consultas select: [:each | each verHora >= (Time hours: 19 minutes: 0 seconds: 0)].
		noche := noche select: [:each | each verHora < (Time hours: 23 minutes: 0 seconds: 0)].
		
		noche do: [:item | 
			sumatoriaEdad := sumatoriaEdad + item verEdad.
			cont := cont + 1.
		]. 
		cont = 0 ifTrue: [
			Transcript show: 'No existen consultas en la noche.'; cr.
		] ifFalse: [
			promedio := sumatoriaEdad / cont.
			Transcript show: 'El promedio de la noche es: ', promedio displayString; cr.
		].
		
		sumatoriaEdad := 0.
		cont := 0.
	].
].

	(opcion = 'Listar consultas reservadas sin contar vacaciones del medico') ifTrue: [
		medicoConsulta := (Prompter prompt: 'Ingrese el nombre del médico a listar consultas').
		fechaInicio := Date fromString: (Prompter prompt: 'Ingrese la fecha de inicio de las vacaciones a excluir (DD/MM/AAAA)').
		fechaFinal := Date fromString: (Prompter prompt: 'Ingrese la fecha del fin de las vacaciones a excluir (DD/MM/AAAA)').
    listadoMedico :=consultorio verTodos select: [:item |item verMed = medicoConsulta].
  
	fechaMenor:=listadoMedico select: [:each | each verFecha < fechaInicio].
	fechaMayor:=listadoMedico select: [:each | each verFecha > fechaFinal].
	fechasNoVacaciones:= fechaMenor, fechaMayor.
	Transcript clear.
    (fechasNoVacaciones isEmpty) ifTrue: [
        MessageBox errorMsg: 'No hay registro de consultas para el médico ', medicoConsulta, ' sin su plazo de vacaciones.'
    ] ifFalse: [
        Transcript show: 'Listado de consultas del médico ', medicoConsulta, ' sin el plazo de vacaciones: ', fechaInicio printString, ' a ', fechaFinal printString; cr.
        listadoMedico do: [:consulta1 |
            Transcript show: 
            'Consulta N°', consulta1 verNum printString; cr;
            show:' | Tipo: ', consulta1 verTipo printString ; cr;
            show:' | Paciente: ', consulta1 verNombre printString , ' '; cr;
            show:' | Médico: ', consulta1 verMed printString ; cr;
            show:' | Fecha: ', consulta1 verFecha printString; cr;
            show:' | Hora: ', consulta1 verHora printString;
            cr.
        ].
        MessageBox notify: 'Se listaron todas las consultas sin el plazo de vacaciones del médico ', medicoConsulta.
    ].
	opcion := nil.
].

	(opcion = 'Generar diccionario de cantidad de consultas por paciente') ifTrue: [
		"Verificar que haya consultas"
		consultas isEmpty ifTrue: [
			MessageBox notify: 'No hay consultas para generar el diccionario.'.
			^self.
		].
		dic := Dictionary new.
		"Recolectamos todos los nombres de los pacientes"
		coleConsulPacien := consultas collect: [:item | item verNombre].
		"Eliminamos los nombres repetidos"
		sinRep := coleConsulPacien asSet.  "Saco los duplicados"
		"Construimos el diccionario con la cantidad de consultas por paciente"
		sinRep do: [:paciente | dic at: paciente put: (coleConsulPacien occurrencesOf: paciente).].
		"Recorremos el diccionario e imprimimos"
		dic keysDo: [:paciente |
		Transcript show: 'Paciente: ', paciente, ' | Cantidad de consultas: ', (dic at: paciente) printString; cr.
		].
		MessageBox notify: 'Diccionario generado exitosamente.'.
].

	(opcion = 'Listar consultas') ifTrue: [
		consultas := consultorio verTodos.
		Transcript clear.
		consultas do: [:urgentesordenadas | 
			  Transcript show: 
			    'Consulta N°', urgentesordenadas verNum printString; cr;
			    show:' | Tipo: ', urgentesordenadas verTipo displayString ; cr;
			    show:' | Paciente: ', urgentesordenadas verNombre printString , ' '; cr;
			    show:' | Médico: ', urgentesordenadas verMed printString ; cr;
			    show:' | Fecha: ', urgentesordenadas verFecha printString; cr;
			    show:' | Hora: ', urgentesordenadas verHora printString;
			    cr.
		].
	].

	(opcion = 'Salir') ifTrue: [
		ejecucion:=false.
	].
].