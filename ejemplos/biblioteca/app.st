| nombre_biblioteca ejecucion menu_principal menu_seleccion menu_estado biblioteca opcion isbn titulo autor editorial estado dni libro identificador hacer delAutor malos dic libros autores sinRepetidos clave |

ejecucion := true.

menu_principal := #('Cargar libro' 'Seleccionar libro' 'Listar todos' 'Listar autor' 'Eliminar malos' 'Libros de los autor').

menu_seleccion := #('Imprimir' 'Modificar' 'Eliminar').

menu_estado := #('Bueno' 'Malo').

nombre_biblioteca := Prompter prompt: 'Nombre de la biblioteca'.

biblioteca := Biblioteca crear: nombre_biblioteca.

[ejecucion] whileTrue:
[
    opcion := ChoicePrompter choices: menu_principal.
    
    (opcion = 'Cargar libro') ifTrue: [
        isbn := ((Random new next * 100) + 1000) ceiling.
        titulo := Prompter prompt: 'Titulo'.
        autor := Prompter prompt: 'Autor'.
        editorial := Prompter prompt: 'Editorial'.
        estado := ChoicePrompter choices: menu_estado.
        dni := (Prompter prompt: 'Dni') asNumber.
        libro := Libro crear: isbn titulo: titulo autor: autor editorial: editorial estado: estado dni: dni.
        biblioteca agregarLibro: libro.
        MessageBox notify: 'Libro Cargado'.
    ].
    
    (opcion = 'Seleccionar libro') ifTrue: [
        identificador := Prompter prompt: 'Ingrese el isbn del libro'.
        libro := biblioteca verTodos detect: [:item | item verIsbn = identificador].
        (libro isNil) ifTrue: [
            MessageBox errorMsg: 'No encontrado'.
        ] ifFalse:[
            hacer := ChoicePrompter choices: menu_seleccion.
            
            (hacer = 'Imprimir') ifTrue: [
                Transcript show: libro verIsbn displayString; cr.
                Transcript show: libro verTitulo; cr.
                Transcript show: libro verAutor; cr.
                Transcript show: libro verEditorial; cr.
                Transcript show: libro verEstado; cr.
                Transcript show: libro verDni displayString; cr.
            ].
            
            (hacer = 'Modificar') ifTrue: [
                titulo := Prompter prompt: 'Nuevo Titulo' ifNil: [libro verTitulo].
                autor := Prompter prompt: 'Nuevo Autor' ifNil: [libro verAutor].
                editorial := Prompter prompt: 'Nueva Editorial' ifNil: [libro verEditorial].
                estado := ChoicePrompter choices: menu_estado ifNil: [libro verEstado].
                libro modificarTitulo: titulo autor: autor editorial: editorial estado: estado.
                MessageBox notify: 'Libro Modificado'.
            ].
            
            (hacer = 'Eliminar') ifTrue: [
                biblioteca eliminarLibro: libro.
                MessageBox notify: 'Libro Eliminado'.
            ].
        ].
    ].
    
    (opcion = 'Listar todos') ifTrue: [
        Transcript clear.
        biblioteca verTodos do: [:item |
            Transcript show: item verIsbn displayString; cr.
            Transcript show: item verTitulo; cr.
            Transcript show: item verAutor; cr.
            Transcript show: item verEditorial; cr.
            Transcript show: item verEstado; cr.
            Transcript show: item verDni displayString; cr.
        ].
    ].
    
    (opcion = 'Listar autor') ifTrue: [
        Transcript clear.
        autor := Prompter prompt: 'Autor'.
        delAutor := biblioteca verTodos select: [:item | item verAutor = autor].
        delAutor do: [:item |
            Transcript show: item verIsbn displayString; cr.
            Transcript show: item verTitulo; cr.
            Transcript show: item verAutor; cr.
            Transcript show: item verEditorial; cr.
            Transcript show: item verEstado; cr.
            Transcript show: item verDni displayString; cr.
        ].
    ].
    
    (opcion = 'Eliminar Malos') ifTrue: [
        Transcript clear.
        malos := biblioteca verTodos reject: [:item | item verEstado = 'Bueno'].
        malos do: [:item |
            Transcript show: item verTitulo, ' Eliminado'; cr.
            biblioteca eliminarLibro: item.
        ].
    ].
    
    (opcion = 'Libros de los autores') ifTrue: [
        Transcript clear.
        dic := Dictionary new.
        libros := biblioteca verTodos.
        autores := libros collect: [:item | item verAutor].
        sinRepetidos := autores asSet.
        sinRepetidos do: [:clave |
            dic at: clave put: (autores occurrencesOf: clave).
        ].
        dic keysDo: [:clave |
            Transcript show: clave , ': ', (dic at: clave) printString; cr.
        ].
    ].
    
    (opcion isNil) ifTrue: [
        ejecucion := false.
    ].
].
nil