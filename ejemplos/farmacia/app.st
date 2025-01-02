| farmacia remedio opcion nombre precio stock laboratorio |

"Inicializar una instancia de la farmacia"
farmacia := Farmacia crear: 'Farmacia Central'.

"Menú interactivo"
[
    Transcript clear.
    Transcript show: 'Bienvenido a la Farmacia! Elige una opción: '; cr.
    Transcript show: '1. Agregar un nuevo remedio'; cr.
    Transcript show: '2. Modificar un remedio'; cr.
    Transcript show: '3. Ver todos los remedios'; cr.
    Transcript show: '4. Ver detalles de un remedio'; cr.
    Transcript show: '5. Salir'; cr.

    "Leer la opción elegida"
    opcion := (Prompter prompt: 'Ingrese la opción:') asNumber.

    (opcion = 1) ifTrue: [
        "Opción 1: Agregar un remedio"
        nombre := Prompter prompt: 'Ingrese el nombre del remedio: '.
        precio := (Prompter prompt: 'Ingrese el precio del remedio: ') asNumber.
        stock := (Prompter prompt: 'Ingrese el stock del remedio: ') asNumber.
        laboratorio := Prompter prompt: 'Ingrese el laboratorio del remedio: '.

        remedio := Remedio new: nombre precio: precio stock: stock laboratorio: laboratorio.
        farmacia agregar: remedio.
        Transcript show: 'Remedio agregado exitosamente!'; cr.
    ].

    (opcion = 2) ifTrue: [
        "Opción 2: Modificar un remedio"
        nombre := Prompter prompt: 'Ingrese el nombre del remedio que desea modificar: '.
        remedio := farmacia conjRem detect: [:r | r verNombre = nombre] ifNone: [nil].

        remedio ifNil: [
            Transcript show: 'Remedio no encontrado'; cr.
        ]. 
	remedio  ifNotNil: [
            precio := (Prompter prompt: 'Ingrese el nuevo precio del remedio: ') asNumber.
            remedio modPrecio: precio.
            Transcript show: 'Precio modificado exitosamente!'; cr.
        ].
    ].

    (opcion = 3) ifTrue: [
        "Opción 3: Ver todos los remedios"
        Transcript show: 'Listado de todos los remedios: '; cr.
        farmacia verTodos do: [:r | Transcript show: r verNombre; cr.].
    ].

    (opcion = 4) ifTrue: [
        "Opción 4: Ver detalles de un remedio"
        nombre := Prompter prompt: 'Ingrese el nombre del remedio para ver sus detalles: '.
        remedio := farmacia conjRem detect: [:r | r verNombre = nombre] ifNone: [nil].

        remedio ifNil: [
            Transcript show: 'Remedio no encontrado'; cr.
        ]. 
	remedio ifNotNil:[
            Transcript show: 'Detalles del remedio: ', 'Nombre: ', remedio verNombre, ', Precio: ', (remedio verPrecio printString), ', Stock: ', (remedio verStock printString), ', Laboratorio: ', remedio verLaboratorio; cr.
        ].
    ].

    (opcion = 5) ifTrue: [
        "Opción 5: Salir"
        Transcript show: 'Saliendo...'; cr.
        false. "Esto termina el ciclo"
    ].

    (opcion < 1 or: [opcion > 5]) ifTrue: [
        Transcript show: 'Opción no válida, elija una opción del 1 al 5'; cr.
    ].
] whileTrue.
