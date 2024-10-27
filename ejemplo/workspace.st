| consola  ejecucion Ids sumatoria promedio dictionary |

"ejecucion"
ejecucion := true.

"Id"
ids := 1.

"Coleccion menu principal - ChoicePrompter"
menuPrincipal := #('Registrar Juego' 'Consultar juego' 'Filtro - Plataforma' 'Filtro - Conectividad' 'Promedio de consumo (Ram)' 'Informe - Genero').
"Coleccion menu secundario - ChoicePrompter"
menuSecundario := #('Eliminar Juego' 'Modificar Juego' 'Inspeccionar Juego').
"Coleccion menu ajustes comun"
menuAjusteComun := #('Titulo' 'Genero' 'Plataforma' 'Tamaño de archivo' 'Huella de memoria').
"Coleccion menu de tipo de juego - ChoicePrompter"
menuJuegos := #('Single Player' 'Multi Player').
"Coleccion menu de genero - ChoicePrompter"
menuGeneros := #('Acción' 'Aventura' 'Rol (RPG)' 'Shooter' 'Estrategia' 'Simulación' 'Deportes' 'Pelea' 'Carreras' 'Plataformas').
"Coleccion menu de plataforma - ChoicePrompter"
menuPlataformas := #('PlayStation 5' 'Xbox Series X' 'Nintendo Switch' 'Atari 2600' 'Nintendo Wii' 'Sega Genesis' 'Super Nintendo').
"Coleccion menu conectividad - ChoicePrompter"
menuConectividad := #('Online' 'Offline').

"Aviso de puesta en marcha"
MessageBox warning: ' - Simulando' caption: '°°°'.

"Nombre de la consola"
nombre := Prompter prompt: '¿Que nombre tendra la consola?' caption:'°°°'.

"Asignar correctamente la memoria"
flag := true.

[ flag == true ] whileTrue: [
	"Memoria de la consola"
	secundaria := [(Prompter prompt: 'Ingrese la cantidad de memoria que tendra la consola' caption: '°°°') asNumber]
		on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
	"Ingreso un 0"
	( secundaria == 0 ) ifTrue: [
		MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
	]. 
	( secundaria > 0) ifTrue: [
		"Todo correcto"
		flag := false.
	].
].

"Instancio la consola"
consola := EmuladorConsola crear: nombre memoria: secundaria.

"MainLoop"
[ ejecucion ] whileTrue: [
	"ChoicePrompter - Menu pricipal"
	opcion := ChoicePrompter choices: menuPrincipal caption: '°°°'.
	"Bifurcacion - Registrar juego"
	( opcion = 'Registrar Juego' ) ifTrue: [
		"Tipo"
		tipo := ChoicePrompter choices: menuJuegos caption: '°°°'.
		"Titulo"
		titulo := Prompter prompt: 'ingrese el titulo del juego'.
		"Genero"
		genero := ChoicePrompter choices: menuGeneros caption: '¬ Genero'.
		"Plataforma"
		plataforma := ChoicePrompter choices: menuPlataformas caption: '¬ Plataforma'.
		"memoria - Secundaria"
		flag := true.
		"Loop - Secundaria"
		[ flag == true ] whileTrue: [
			"Memoria Secundaria"
			secundaria := [(Prompter prompt: 'Ingrese el tamaño de archivo - (Disco)' caption: '°°°') asNumber]
				on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
			"Ingreso un 0"
			( secundaria == 0 ) ifTrue: [
				MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
			]. 
			( secundaria > 0) ifTrue: [
				"Todo correcto"
				flag := false.
			].
		].
		"Memoria - Primaria"
		flag := true.
		"Loop - Primaria"
		[ flag == true ] whileTrue: [
			"Memoria Primaria"
			primaria := [(Prompter prompt: 'Ingrese la huella de memoria - (Ram)' caption: '°°°') asNumber]
				on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
			"Ingreso un 0"
			( primaria == 0 ) ifTrue: [
				MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
			]. 
			( primaria > 0) ifTrue: [
				"Todo correcto"
				flag := false.
			].
		].
		"Tipo - Single Player"
		( tipo = 'Single Player') ifTrue: [
			"Duracion"
			flag := true.
			"Loop - Duracion"
			[ flag == true ] whileTrue: [
				"Duracion"
				duracion := [(Prompter prompt: 'Duracion de la campaña' caption: '°°°') asNumber]
					on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
				"Ingreso un 0"
				( duracion == 0 ) ifTrue: [
					MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
				]. 
				( duracion > 0) ifTrue: [
					"Todo correcto"
					flag := false.
				].
			].
			"Instancia - Single Player"
			juego := SinglePlayer crear: ids titulo: titulo genero: genero plataforma: plataforma peso: secundaria consumo: primaria duracion: duracion.
		].
		"Tipo - Multi Player"
		( tipo = 'Multi Player' ) ifTrue: [
			"Maximo jugadores"
			flag := true.
			"Loop - Maximo Jugadores"
			[ flag == true ] whileTrue: [
				"Maximo"
				maximo := [(Prompter prompt: 'Maximo de jugadores' caption: '°°°') asNumber]
					on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
				"Ingreso un 0"
				( maximo == 0 ) ifTrue: [
					MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
				]. 
				( maximo > 0) ifTrue: [
					"Todo correcto"
					flag := false.
				].
			].
			"Online - YesNo"
			online := MessageBox confirm: '¿Es online?'.
			"instancia - Multi Player"
			juego := MultiPlayer crear: ids titulo: titulo genero: genero plataforma: plataforma peso: secundaria consumo: primaria maximo: maximo online: online.
		].
		"Espacio Libre"
		libre := consola verLibre.
		( libre >= juego verPeso ) ifTrue: [
			"Agregar Juego"
			consola agregarJuego: juego.
			"Actualizo el espacio libre"
			consola modLibre: consola verLibre - juego verPeso.
			"Actualizo el espacio utilizado"
			consola modUsado: consola verUsado + juego verPeso.
			"Incremento id - *Para el Proximo juego*"
			ids := ids + 1.
			MessageBox notify: 'Juego Cargado' caption: '°°°'.
		] ifFalse: [
			"No se cuenta con suficiente espacio"
			MessageBox errorMsg: 'Fallo: No hay suficiente memoria' caption: '°°°'.
		].
	].
	"Bifurcacion - Consultar Juego"
	( opcion = 'Consultar juego' ) ifTrue: [
		"Catalogo"
		catalogo := consola verTodos.
		"ChoicePrompter - Menu secundario"
		opcion := ChoicePrompter choices: menuSecundario caption: '°°°'.
		"Se eligio una opcion"
		( opcion = nil ) ifFalse: [
			"Consultar (Juego -> id)"
			flag := true.
			"Loop - id"
			[ flag == true ] whileTrue: [
				"id"
				id := [(Prompter prompt: 'Ingrese una id para consultar por un juego' caption: '°°°') asNumber]
					on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
				"Ingreso un 0"
				( id == 0 ) ifTrue: [
					MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
				]. 
				( id > 0) ifTrue: [
					"Todo correcto"
					flag := false.
				].
			].
			"Filtro por id"
			[
				juego := catalogo detect: [:item | item verId = id] ifNone: [nil].
			] on: Error do: [:ex |
				MessageBox errorMsg: 'No encontrado'.
			].
			"Juego encontrado"
			( juego = nil ) ifFalse: [
				"Bifurcacion - Eliminar Juego"
				( opcion = 'Eliminar Juego' ) ifTrue: [	
					"Obtengo tamaño de archivo"
					secundaria := juego verPeso.
					"Elimino el juego"
					consola elminarJuego: juego.
					"Actualizo el espacio libre"
					consola modLibre: consola verLibre + secundaria.
					"Actualizo el espacio utilizado"
					consola modUsado: consola verUsado - secundaria.
					MessageBox notify: 'Juego Eliminado' caption: '°°°'.
				].
				"Bifurcacion - Modificar Juego"
				( opcion = 'Modificar Juego' ) ifTrue: [
					"Modificar - Single Player"
					( juego isKindOf: SinglePlayer ) ifTrue: [
						"Coleccion menu ajustes Single Player"
						menuAjusteSP := menuAjusteComun , #('Duracion').
						"ChoicePrompter - Menu Ajuste Single Player"
						opcion := ChoicePrompter choices: menuAjusteSP caption: '°°°'.
						"Modificar Titulo"
						( opcion = 'Titulo') ifTrue: [
							titulo := Prompter prompt: 'Ingrese el nuevo titulo' caption: '°°°'.
							juego modTitulo: titulo.
						].
						"Modificar Genero"
						( opcion  = 'Genero' ) ifTrue: [
							genero := ChoicePrompter choices: menuGeneros caption: '¬ Genero'.
							juego modGenero: genero.
						].
						"Modificar Plataforma"
						( opcion  = 'Plataforma' ) ifTrue: [
							plataforma := ChoicePrompter choices: menuPlataformas caption: '¬ Genero'.
							juego modPlataforma: plataforma.
						].
						"Modificar Tamaño de archivo"
						( opcion  = 'Tamaño de archivo' ) ifTrue: [
							"memoria - Secundaria"
							flag := true.
							"Loop - Secundaria"
							[ flag == true ] whileTrue: [
								"Memoria Secundaria"
								secundaria := [(Prompter prompt: 'Ingrese el nuevo tamaño de archivo - (Disco)' caption: '°°°') asNumber]
									on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
								"Ingreso un 0"
								( secundaria == 0 ) ifTrue: [
									MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
								]. 
								( secundaria > 0) ifTrue: [
									"Todo correcto"
									flag := false.
								].
							].
							"Espacio actual"
							actual := juego verPeso.
							"Espacio sin el juego"
							sin := consola verLibre + actual.
							( sin >= secundaria ) ifTrue: [
								"Actualizo el espacio libre"
								consola modLibre: sin - secundaria.
								"Actualizo el espacio utilizado"
								consola modUsado: consola verUsado + secundaria .
								"Modifico"
								juego modPeso: secundaria.
							] ifFalse: [
								"No se cuenta con suficiente espacio"
								MessageBox errorMsg: 'Fallo: No hay suficiente memoria' caption: '°°°'.
							].							
						].
						"Modificar Huella de memoria"
						( opcion  = 'Huella de memoria' ) ifTrue: [
							"Memoria - Primaria"
							flag := true.
							"Loop - Primaria"
							[ flag == true ] whileTrue: [
								"Memoria Primaria"
								primaria := [(Prompter prompt: 'Ingrese la nueva huella de memoria - (Ram)' caption: '°°°') asNumber]
									on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
								"Ingreso un 0"
								( primaria == 0 ) ifTrue: [
									MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
								]. 
								( primaria > 0) ifTrue: [
									"Todo correcto"
									flag := false.
								].
							].
							juego modPeso: primaria.
						].
						"Modificar Duracion de la campaña"
						( opcion = 'Duracion' ) ifTrue: [
							"Duracion"
							flag := true.
							"Loop - Duracion"
							[ flag == true ] whileTrue: [
								"Duracion"
								duracion := [(Prompter prompt: 'Duracion de la campaña' caption: '°°°') asNumber]
									on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
								"Ingreso un 0"
								( duracion == 0 ) ifTrue: [
									MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
								]. 
								( duracion > 0) ifTrue: [
									"Todo correcto"
									flag := false.
								].
							].
							juego modDuracion: duracion.
						].
					].
					"Modificar - Multi Player"
					( juego isKindOf: MultiPlayer ) ifTrue: [
						"Establecer menu - Multi Player"
						menuAjusteMP := menuAjusteComun , #('Maximo' 'Online').
						"ChoicePrompter - Menu Ajuste Multi Player"
						opcion := ChoicePrompter choices: menuAjusteSP caption: '°°°'.
						"Modificar Titulo"
						( opcion = 'Titulo') ifTrue: [
							titulo := Prompter prompt: 'Ingrese el nuevo titulo' caption: '°°°'.
							juego modTitulo: titulo.
						].
						"Modificar Genero"
						( opcion  = 'Genero' ) ifTrue: [
							genero := ChoicePrompter choices: menuGeneros caption: '¬ Genero'.
							juego modGenero: genero.
						].
						"Modificar Plataforma"
						( opcion  = 'Plataforma' ) ifTrue: [
							plataforma := ChoicePrompter choices: menuPlataformas caption: '¬ Plataforma'.
							juego modPlataforma: plataforma.
						].
						"Modificar Tamaño de archivo"
						( opcion  = 'Tamaño de archivo' ) ifTrue: [
							"memoria - Secundaria"
							flag := true.
							"Loop - Secundaria"
							[ flag == true ] whileTrue: [
								"Memoria Secundaria"
								secundaria := [(Prompter prompt: 'Ingrese el nuevo tamaño de archivo - (Disco)' caption: '°°°') asNumber]
									on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
								"Ingreso un 0"
								( secundaria == 0 ) ifTrue: [
									MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
								]. 
								( secundaria > 0) ifTrue: [
									"Todo correcto"
									flag := false.
								].
							].
							"Espacio actual"
							actual := juego verPeso.
							"Espacio sin el juego"
							sin := consola verLibre + actual.
							( sin >= secundaria ) ifTrue: [
								"Actualizo el espacio libre"
								consola modLibre: sin - secundaria.
								"Actualizo el espacio utilizado"
								consola modUsado: consola verUsado + secundaria .
								"Modifico"
								juego modPeso: secundaria.
							] ifFalse: [
								"No se cuenta con suficiente espacio"
								MessageBox errorMsg: 'Fallo: No hay suficiente memoria' caption: '°°°'.
							].	
						].
						"Modificar Huella de memoria"
						( opcion  = 'Huella de memoria' ) ifTrue: [
							"Memoria - Primaria"
							flag := true.
							"Loop - Primaria"
							[ flag == true ] whileTrue: [
								"Memoria Primaria"
								primaria := [(Prompter prompt: 'Ingrese la nueva huella de memoria - (Ram)' caption: '°°°') asNumber]
									on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
								"Ingreso un 0"
								( primaria == 0 ) ifTrue: [
									MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
								]. 
								( primaria > 0) ifTrue: [
									"Todo correcto"
									flag := false.
								].
							].
							juego modPeso: primaria.
						].
						( opcion  = 'Maximo' ) ifTrue: [
							"Maximo jugadores"
							flag := true.
							"Loop - Maximo Jugadores"
							[ flag == true ] whileTrue: [
								"Maximo"
								maximo := [(Prompter prompt: 'Maximo de jugadores' caption: '°°°') asNumber]
									on: Error do: [:ex | MessageBox errorMsg: 'Fallo: No ingreso un numero entero' caption: '°°°'. -1.].
								"Ingreso un 0"
								( maximo == 0 ) ifTrue: [
									MessageBox warning: 'El valor 0 no es aceptado' caption: '°°°'.
								]. 
								( maximo > 0) ifTrue: [
									"Todo correcto"
									flag := false.
								].
							].
							juego modMaximo: maximo.
						].
						( opcion = 'Online' ) ifTrue: [
							"Online - YesNo"
							online := MessageBox confirm: '¿Es online?' caption: '°°°'.
							juego modOnline: online.
						].
					].
				"Fue Modificado"
				MessageBox notify: 'Saliendo de modificar' caption: '°°°'.
				].
				( opcion = 'Inspeccionar Juego' ) ifTrue: [
					Transcript clear.
					Transcript show: 'Titulo: ', juego verTitulo. Transcript cr.
					Transcript show: 'Plataforma: ',juego verPlataforma. Transcript cr.
					Transcript show: 'Genero: ',juego verGenero. Transcript cr.
					Transcript show: 'Peso: ', (juego verPeso) displayString. Transcript cr.
					Transcript show: 'Consumo: ', (juego verConsumo) displayString. Transcript cr.
					
				].
			].
			(juego = nil ) ifTrue: [
				"No fue encontrado"
				MessageBox warning: 'Juego no encontrado' caption: '°°°'.
			].	
		].
	].
	"Bifurcacion - Filtro - Plataforma"
	( opcion = 'Filtro - Plataforma' ) ifTrue: [
		"ChoicePrompter - Menu Plataforma"
		plataforma := ChoicePrompter choices: menuPlataformas caption: '¬ Plataforma'.
		"Catalogo"
		catalogo := consola verTodos.
		"Filtro - Plataforma"
		filtrado := catalogo select: [:item | item verPlataforma = plataforma].
		"Imprimo los filtrados"
		filtrado do: [:item | MessageBox notify: item verTitulo].
	].
	"Bifurcacion - Filtro - Conectividad"
	( opcion = 'Filtro - Conectividad' ) ifTrue: [
		"ChoicePrompter - Menu Conectividad"
		conectividad := ChoicePrompter choices: menuConectividad caption: '¬ Conectividad'.
		( conectividad  = 'Online' ) ifTrue: [
			conectividad := true.
		] ifFalse: [
			conectividad := false.
		].
		"Catalogo"
		catalogo := consola verTodos.
		"Catalogo - Multi Player"
		catalogoMultiPlayer := catalogo select: [:item | item isKindOf: MultiPlayer].
		"Existe por lo menos un multiplayer"
		( catalogoMultiPlayer isEmpty ) ifFalse: [
			"Filtro - Conectividad"
			filtrado := catalogoMultiPlayer select:[:item | item verOnline = conectividad].
		].
		"Si criterio - Offline"
		( conectividad = false ) ifTrue: [
			"Catalogo - Single Player"
			catalogoSinglePlayer := catalogo select: [:item | item isKindOf: SinglePlayer].
			"Filtro - Offline"
			filtrado addAll: catalogoSinglePlayer.
		].
		"Imprimo los filtrados"
		filtrado do: [:item | MessageBox notify: item verTitulo].
	].
	"Bifurcacion - Promedio de consumo (Ram)"
	( opcion = 'Promedio de consumo (Ram)' ) ifTrue: [
		"Catalogo"
		catalogo := consola verTodos.
		"Cantidad de juegos"
		cantidad := consola tamanio.
		"Inicializacion - Sumatoria - Consumo (Ram)"
		sumatoria := 0.
		"Calculo - sumatoria"
		catalogo do: [:item | sumatoria := sumatoria + item verConsumo].
		"Calculo - promedio"
		promedio := sumatoria / cantidad.
		"Imprimir promedio - Transcript show: promedio asString"
		MessageBox notify: promedio displayString.
	].
	"Bifurcacion - Informe - Genero"
	( opcion = 'Informe - Genero' ) ifTrue: [
		"Inicializo el diccionario"
		dictionary := Dictionary new.
		"Catalogo"
		catalogo := consola verTodos.
		"Plataformas - Repetidas"
		plataformas := catalogo collect: [:item | item verPlataforma].
		"Plataformas - Sin Repetir"
		set := plataformas asSet.
		"Poblar Diccionario"
		set do: [:item | 
			dictionary at: item put:(plataformas occurrencesOf: item).
		].
		"Imprimir Clave - Valor | dictionary keysDo:[:clave | Transcript show: 'Plataforma: ', clave, 'Cantidad: ' , dictionary at:clave. Transcript cr]."
		Transcript clear.
		dictionary keysDo:[:clave | Transcript show: 'Plataforma: ', clave, '     Cantidad: ' , (dictionary at:clave)displayString. Transcript cr].
	].	
	"Finalizar Programa"
	( opcion = nil ) ifTrue: [
		cerrar := MessageBox confirm: '¿Estas seguro?' caption: '°°°'.
		( cerrar = true ) ifTrue: [
			ejecucion:=false.
		].
	].
].
"Fin programa"