| empleado1 empleado2 proyecto1 proyecto2 |

"Crear empleados"
empleado1 := Empleado new init: 'Juan'.
empleado2 := Empleado new init: 'Ana'.

"Crear proyectos"
proyecto1 := Proyecto new init: 'Desarrollo Web' fecha: '2024-01-01'.
proyecto2 := Proyecto new init: 'Investigación IA' fecha: '2024-06-01'.

"Asignar proyectos a empleados"
empleado1 asignarProyecto: proyecto1.
empleado2 asignarProyecto: proyecto2.
empleado2 asignarProyecto: proyecto1.  "Ana también trabaja en el proyecto Desarrollo Web"

"Asignar empleados a proyectos"
proyecto1 asignarEmpleado: empleado1.
proyecto1 asignarEmpleado: empleado2.
proyecto2 asignarEmpleado: empleado2.

"Ver los proyectos de cada empleado"
Transcript show: empleado1 nombre, ' está trabajando en los proyectos: ', empleado1 verProyectos; nl.
Transcript show: empleado2 nombre, ' está trabajando en los proyectos: ', empleado2 verProyectos; nl.

"Ver los empleados en cada proyecto"
Transcript show: proyecto1 verNombre, ' tiene los siguientes empleados: ', 
    (proyecto1 empleados collect: [ :empleado | empleado nombre ]) joinSeparatedBy: ', '; nl.
Transcript show: proyecto2 verNombre, ' tiene los siguientes empleados: ', 
    (proyecto2 empleados collect: [ :empleado | empleado nombre ]) joinSeparatedBy: ', '; nl.
