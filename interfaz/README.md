# Interfaces en Smalltalk

Smalltalk provee varias interfaces útiles para interactuar con el usuario y gestionar la entrada y salida de datos. A continuación se explican algunas de las interfaces más comunes y su función:

## choicePrompter.st

La interfaz `choicePrompter.st` permite al usuario seleccionar una opción de una lista de opciones predefinidas. Esta interfaz es útil cuando se requiere que el usuario haga una selección entre varias opciones disponibles. Normalmente, se presenta como un cuadro de diálogo que muestra las opciones y espera la respuesta del usuario.

### Funcionalidad:
- Presenta una lista de opciones al usuario.
- Permite seleccionar una opción a través de una interfaz gráfica o en la consola.
- Devuelve la opción seleccionada por el usuario.

## messageBox.st

La interfaz `messageBox.st` se utiliza para mostrar mensajes al usuario en forma de cuadros de diálogo. Esta interfaz es comúnmente utilizada para mostrar información, advertencias o errores de manera visual.

### Funcionalidad:
- Muestra un mensaje al usuario en un cuadro de diálogo.
- Permite mostrar mensajes de información, advertencias o errores.
- El usuario puede interactuar con el mensaje (por ejemplo, cerrando el cuadro de diálogo).

## prompter.st

La interfaz `prompter.st` permite pedir al usuario que ingrese algún dato a través de un cuadro de texto. Es útil cuando se requiere que el usuario ingrese información, como nombres, números u otros datos.

### Funcionalidad:
- Muestra un cuadro de texto donde el usuario puede ingresar información.
- Se puede personalizar con un mensaje que indique al usuario qué tipo de datos debe ingresar.
- Devuelve el dato ingresado por el usuario.

## transcript.st

La interfaz `transcript.st` se utiliza para mostrar información al usuario en un área de texto. A diferencia de los cuadros de mensaje, `transcript.st` permite una salida continua de texto, lo que es útil para mostrar logs, depuración o información dinámica en tiempo real.

### Funcionalidad:
- Muestra información en un área de texto persistente.
- Utilizada comúnmente para depuración y salida de logs.
- Permite visualizar información dinámica durante la ejecución del programa.
