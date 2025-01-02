 |dolphinName dolphinVersion |

"Obtenemos el nombre y la versión de Dolphin"
dolphinName := 'Dolphin Smalltalk'.
dolphinVersion := 'Versión 7.0.5'. 

"Primer MessageBox con el nombre de Dolphin"
MessageBox notify: 'Nombre de Dolphin: ', dolphinName.

"Segundo MessageBox con la versión de Dolphin"
MessageBox notify: 'Versión de Dolphin: ', dolphinVersion.

"MessageBox de confirmación"
MessageBox notify: '¡Todo funciona correctamente!'.
