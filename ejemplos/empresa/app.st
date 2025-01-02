| empleado1 empleado2 empleado3 empleado4 empleado5 |

empresa := Empresa crearEmpresa: 'Globant'.

"Empleados permanentes"
empleado1 := Permanente crear: 'Juan Perez' con: 2 con: 160 con: true con: 'Casado' con: 5.
empleado2 := Permanente crear: 'Maria Lopez' con: 1 con: 150 con: false con: 'Soltero' con: 3.

"Empleados temporarios"
empleado3 := Temporario crear: 'Carlos Diaz' con: 3 con: 140 con: false con: 'Casado' con: 'Contrato a término'.
empleado4 := Temporario crear: 'Laura Gómez' con: 2 con: 130 con: true con: 'Soltero' con: 'Contrato a término'.
empleado5 := Temporario crear: 'Pedro Martinez' con: 1 con: 120 con: false con: 'Casado' con: 'Contrato a término'.

empresa agregarEmpleado: empleado1.
empresa agregarEmpleado: empleado2.
empresa agregarEmpleado: empleado3.
empresa agregarEmpleado: empleado4.
empresa agregarEmpleado: empleado5.

"De licencia"
empleados := empresa verEmpleados.
licencias := empleados select: [:each | each verLic = true ].
sumatoria := 0.
licencias do: [:each | 
		sumatoria:= sumatoria + each sueldo.
].
Transcript show: sumatoria displayString .

"Temporarios"
empleados := empresa verEmpleados.
temporarios := empleados select: [:each | each isKindOf: Temporario ].
sumatoria := 0.
temporarios do: [:each | 
		sumatoria:= sumatoria + each sueldo.
].
Transcript show: sumatoria displayString.

"Salario familiar"
empleados := empresa verEmpleados.
familiar := empleados select: [:each | each verHijos > 0 ].
sumatoria := 0.
familiar do: [:each | 
		sumatoria:= sumatoria + each salario. 
].
Transcript show: sumatoria displayString.

"Diccionario"
dic := Dictionary new.
dic at: 'Permanente' put: (empleados select: [:each | each isKindOf: Permanente ]) size.
dic at: 'Temporario' put: (empleados select: [:each | each isKindOf: Temporario ]) size.

dic inspect.