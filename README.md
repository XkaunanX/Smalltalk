# Smalltalk

Smalltalk es un lenguaje de programacion orientado a objetos puro, conocido por su simplicidad, consistencia y enfoque en la interactividad. Fue una de las primeras implementaciones practicas de la programacion orientada a objetos y establecio muchas de las bases para los lenguajes modernos.

## En que contexto nacio Smalltalk

Smalltalk fue desarrollado en Xerox PARC en la decada de 1970 bajo la direccion de Alan Kay. Su objetivo principal era crear un lenguaje que promoviera la exploracion, el aprendizaje interactivo y la investigacion en interfaces graficas de usuario. Smalltalk fue una revolucion en el diseño de software, influyendo en lenguajes y sistemas modernos.

## Caracteristicas Principales

### Nivel del lenguaje
Smalltalk es un lenguaje de alto nivel, lo que permite a los desarrolladores enfocarse en la solucion de problemas en lugar de los detalles tecnicos de hardware o gestion de memoria.

```smalltalk
precios := #(100 200 300).
descuento := 0.1.
total := (precios collect: [:precio | precio * (1 - descuento)]) sum.
```

### Uso de punteros
Smalltalk no requiere manejo explicito de punteros. La memoria y las referencias son gestionadas automaticamente por la maquina virtual.

```smalltalk
Nodo subclass: #Nodo
    instanceVariableNames: 'valor siguiente'.

nodo1 := Nodo new.
nodo1 valor: 10.

nodo2 := Nodo new.
nodo2 valor: 20.

nodo1 siguiente: nodo2.
```

### Orientacion a objetos puro
Smalltalk es un lenguaje orientado a objetos puro, donde todo es un objeto. Los numeros, cadenas, clases, bloques de codigo y metodos son tratados uniformemente como objetos.

```smalltalk
"Definimos una clase para representar empleados"
Object subclass: #Empleado
    instanceVariableNames: 'nombre tarifa tareas'.

Empleado>>initialize
    nombre := ''.
    tarifa := 0.
    tareas := OrderedCollection new.

Empleado>>nombre: unNombre
    nombre := unNombre.

Empleado>>tarifa: unaTarifa
    tarifa := unaTarifa.

Empleado>>asignarTarea: unaTarea
    tareas add: unaTarea.

Empleado>>calcularCosto
    ^tareas inject: 0 into: [:suma :tarea | suma + (tarea costo * tarifa)].

"Tarea como bloque de codigo"
Object subclass: #Tarea
    instanceVariableNames: 'nombre costo'.

Tarea>>initialize
    nombre := ''.
    costo := 0.

Tarea>>nombre: unNombre
    nombre := unNombre.

Tarea>>costo: unCosto
    costo := unCosto.

"Creacion y gestion de objetos"
empleado := Empleado new.
empleado nombre: 'Ana'.
empleado tarifa: 50.

tarea1 := Tarea new.
tarea1 nombre: 'Analisis'.
tarea1 costo: 3.

tarea2 := Tarea new.
tarea2 nombre: 'Desarrollo'.
tarea2 costo: 5.

empleado asignarTarea: tarea1.
empleado asignarTarea: tarea2.

"Calculamos el costo total de las tareas asignadas"
totalCosto := empleado calcularCosto.
```

### Mensajes
En Smalltalk, las interacciones entre objetos ocurren mediante el envio de mensajes. Los mensajes son solicitudes que se envian a un objeto receptor para que realice una accion. Existen tres tipos principales de mensajes:
- **Unarios**: Mensajes simples que no tienen parametros. Ejemplo: `objeto.nombre`.
- **Binarios**: Mensajes que implican operaciones entre dos objetos. Ejemplo: `5 + 3`.
- **De palabra clave**: Mensajes con uno o mas parametros. Ejemplo: `objeto insertar: valor en: posicion`.

```smalltalk
"Definimos una coleccion de numeros"
numeros := #(1 2 3 4 5).

"Mensaje unario: calcular el tamaño de la coleccion"
tamanio := numeros size.  "Devuelve 5"

"Mensaje binario: sumar un numero a otro"
suma := 10 + 5.  "Devuelve 15"

"Mensaje de palabra clave: agregar un elemento en una posicion especifica"
coleccion := OrderedCollection new.
coleccion add: 42 at: 1.  "Agrega el numero 42 en la posicion 1"
```

### Herencia
Smalltalk utiliza herencia simple, donde cada clase tiene un unico ancestro directo. Esta herencia puede incluir atributos y metodos, proporcionando una herencia completa.

# Herencia en Smalltalk

Smalltalk utiliza herencia simple, lo que significa que cada clase tiene un unico ancestro directo. Esta herencia puede incluir tanto atributos como metodos, lo que permite la reutilizacion y extension de comportamiento de manera eficiente.

## Ejemplo de Herencia Simple

Imaginemos un escenario en un zoologico donde modelamos diferentes tipos de animales. La jerarquia de clases seria la siguiente:

1. **Clase base: Animal**  
   La clase `Animal` define comportamientos y atributos comunes a todos los animales, como `respirar`, `comer`, y un atributo `edad`. Todos los animales, sin importar el tipo, comparten estos comportamientos.

2. **Clase derivada: Mamifero**  
   La clase `Mamifero` hereda de `Animal` y añade comportamientos especificos, como `amamantar`. Al ser herencia simple, `Mamifero` tiene un unico ancestro directo, que es `Animal`.

3. **Clase derivada: Perro**  
   La clase `Perro` hereda de `Mamifero` y añade comportamientos adicionales, como `ladrar`. A traves de esta herencia, un `Perro` tiene acceso a los comportamientos de `Mamifero` y `Animal`.

### Explicacion de la Herencia:
- Un objeto de la clase `Perro` tiene acceso a todos los metodos y atributos de las clases `Perro`, `Mamifero`, y `Animal`. Por ejemplo:
  - Puede `ladrar` (definido en `Perro`).
  - Puede `amamantar` (definido en `Mamifero`).
  - Puede `respirar` y `comer` (definidos en `Animal`).

### Interpretacion de que todo sea objeto
Todo en Smalltalk es un objeto, lo que significa que incluso las clases mismas son objetos. Esto permite que:
- Los bloques de codigo sean tratados como objetos, lo que habilita estructuras y comportamiento dinamicos.
- Los tipos de datos se interpreten como objetos, proporcionando un sistema uniforme.

```smalltalk
"Crear una lista con distintos tipos de objetos"
miLista := #(1 'cadena' #(1 2 3) [ :x | x * 2 ]).

"Acceder a los elementos, cada uno es un objeto"
miLista at: 1.  "Accede al número 1"
miLista at: 2.  "Accede a la cadena 'cadena'"
miLista at: 3.  "Accede a la lista #(1 2 3)"
miLista at: 4.  "Accede al bloque de código [ :x | x * 2 ]"

"Enviar un mensaje al bloque de código para ejecutarlo"
resultado := (miLista at: 4) value: 5.  "Ejecuta el bloque con el valor 5, devuelve 10"
```

# Clase `Object` y Árbol de Herencia en Smalltalk

En Smalltalk, **`Object`** es la clase raíz de todas las clases. Esto significa que cada clase hereda directa o indirectamente de `Object`, lo que establece una base común para todas las clases del lenguaje. El árbol de herencia define la jerarquía y las relaciones entre clases, permitiendo que todas las clases compartan un conjunto básico de comportamientos.

## Clase `Object`

La clase `Object` es la superclase de todas las clases en Smalltalk, y proporciona métodos fundamentales que son heredados por todas las demás clases. Algunos de los métodos más comunes de `Object` incluyen:
- `==`: Compara dos objetos para verificar si son el mismo objeto.
- `hash`: Devuelve un valor numérico que representa el objeto.
- `printOn:`: Imprime una representación del objeto.

Cada clase en Smalltalk, ya sea creada por el sistema o por el programador, hereda estos métodos de `Object`, lo que asegura que todas las clases tengan ciertas propiedades en común.

## Árbol de Herencia

El árbol de herencia en Smalltalk refleja las relaciones jerárquicas entre las clases, donde `Object` ocupa la raíz. A medida que avanzamos en el árbol, las clases se especializan y amplían los comportamientos definidos en sus clases padre. Un ejemplo simple de un árbol de herencia sería:

      Object
         |  
    ------------    
    |          |  
  Animal     Vehiculo  
    |           |  
----------   ----------  
|        |   |        |  
Mamifero Ave Coche   Barco
### Explicación del Árbol de Herencia:

1. **`Object`**: Es la raíz de todo, todas las clases heredan de `Object`.
2. **`Animal`** y **`Vehiculo`**: Son clases que heredan de `Object` y actúan como superclases para sus respectivas subclases.
3. **`Mamifero`** y **`Ave`**: Son subclases de `Animal` que especializan el comportamiento de `Animal` para definir animales más específicos.
4. **`Coche`** y **`Barco`**: Son subclases de `Vehiculo` que especializan el comportamiento de vehículos.

### Metodos de clase y metodos de instancia
- **Metodos de clase**: Definen el comportamiento asociado con la clase como un todo.
- **Metodos de instancia**: Definen el comportamiento asociado con objetos especificos creados a partir de una clase.

### Cambio de clases en tiempo de ejecucion
En Smalltalk, las clases pueden ser modificadas dinamicamente en tiempo de ejecucion, lo que facilita la adaptacion y experimentacion sin necesidad de reiniciar el sistema.

### Archivos de clases y workspace
- **Archivos de clases**: Contienen definiciones de clases, metodos y propiedades.
- **Archivos de workspace**: Permiten escribir y probar codigo interactivamente en un entorno de desarrollo.

### Tipo de tipado
Smalltalk utiliza tipado dinamico, donde los tipos de las variables se determinan en tiempo de ejecucion, permitiendo gran flexibilidad.

### Compilado o interpretado
Smalltalk es un lenguaje interpretado. Utiliza una maquina virtual para ejecutar codigo en tiempo real, ofreciendo un entorno altamente interactivo.

