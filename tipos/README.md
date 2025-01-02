# Tipos de Datos en Smalltalk

Smalltalk es un lenguaje dinamico que no requiere declaracion explicita de tipos. Los tipos de datos se determinan en tiempo de ejecucion, lo que brinda flexibilidad en el desarrollo de aplicaciones. A continuacion, se describen los tipos de datos mas comunes en Smalltalk.

## Tipos de Datos Primitivos

1. **Booleanos**
   - Los valores booleanos en Smalltalk son representados por los objetos `true` y `false`.
   - Ejemplo: `true`, `false`.

2. **Numeros**
   - Smalltalk tiene varios tipos numericos que permiten trabajar con enteros y decimales:
     - **Enteros**: Los numeros enteros en Smalltalk se representan con objetos como `5`, `100`, `-23`.
     - **Flotantes**: Los numeros con decimales se representan con objetos como `3.14`, `-2.71`.

3. **Cadenas de Texto (String)**
   - Las cadenas de texto en Smalltalk son objetos de la clase `String` y se representan con comillas simples.
   - Ejemplo: `'Hola Mundo'`, `'Smalltalk es genial'`.

4. **Simbolos**
   - Los simbolos en Smalltalk son utilizados principalmente como identificadores y se definen con un prefijo `#`.
   - Los simbolos son inmutables y comparables rapidamente, lo que los hace ideales para ser utilizados como claves en diccionarios.
   - Ejemplo: `#nombre`, `#edad`.

5. **Objetos**
   - En Smalltalk, todo es un objeto, lo que incluye numeros, cadenas y simbolos. Los objetos son instancias de clases y pueden tener atributos y metodos.
   - Ejemplo: `Object new`, `String new`.

6. **Fechas y Horas**
   - Smalltalk tiene clases especificas para representar fechas y horas, como `Date` y `Time`. Estas clases permiten trabajar con fechas y horas de manera eficiente.
   - Ejemplo: `Date today`, `Time now`.

## Tipado Dinamico

En Smalltalk, las variables no tienen un tipo fijo. El tipo de una variable se determina en tiempo de ejecucion, lo que significa que una misma variable puede almacenar diferentes tipos de valores a lo largo del programa.

Ejemplo de tipado dinamico:

```smalltalk
| variable |
variable := 10. "Entero"
Transcript show: 'Valor de variable: ', variable printString; cr.

variable := 'Hola'. "Cadena de texto"
Transcript show: 'Valor de variable: ', variable printString; cr.

variable := Object new. "Objeto"
Transcript show: 'Valor de variable: ', variable printString; cr.
```

# Jerarquia de tipos

```plaintext
Object
   |
   +-- Magnitude
   |     |
   |     +-- Number
   |     |     |
   |     |     +-- Float
   |     |     +-- Integer
   |     |     |     |
   |     |     |     +-- SmallInteger
   |     |     |     +-- LargePositiveInteger
   |     |     |     +-- LargeNegativeInteger
   |     |     |
   |     |     +-- Fraction
   |     |
   |     +-- Character
   |     +-- Date
   |     +-- Time
   |
   +-- Boolean
```
