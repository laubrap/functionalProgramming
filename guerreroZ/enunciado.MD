## GuerreroZ

En la galaxia, las fuerzas de Freezer están en constante expansión, conquistando planetas y enfrentándose a los guerreros liderados por Goku. Para prepararse mejor, los Guerreros Z decidieron construir un sistema que les permita registrar y analizar combatientes, sus transformaciones y el estado de los equipos.

1. Para comenzar, necesitamos modelar a los **guerreros** del universo de Dragon Ball. De cada Guerrero sabemos:
   - nombre
   - nivel de fuerza
   - raza (Saiyajin, Demonio, Humano o Namekusein)
   - cantidad de transformaciones realizadas

2. Definir a los siguientes guerreros:

   | | | |
   |---|---|---|
   | **Goku**<br>Nivel de fuerza: 300<br>Raza: Saiyajin<br>Transformaciones: 1 | **Piccolo**<br>Nivel de fuerza: 150<br>Raza: Namekusein<br>Transformaciones: 2 | **Yamcha**<br>Nivel de fuerza: 50<br>Raza: Humano<br>Transformaciones: 0 |
   | **Freezer**<br>Nivel de fuerza: 10000<br>Raza: Demonio<br>Transformaciones: 1 | **Dodoria**<br>Nivel de fuerza: 10000<br>Raza: Demonio<br>Transformaciones: 1 | 

3. Hacer que un guerrero **coma una semilla de ermitaño**. Esto le hace incrementar su fuerza en 4000, y le agrega "Super " al principio de su nombre.
   a. Ejemplo: Goku al comer una semilla su nombre cambia a "Super Goku" y tiene 4300 de fuerza.

4. Hacer que un guerrero se **transforme**, lo cual significa incrementar su fuerza y su cantidad de transformaciones realizadas. El incremento de fuerza depende de la raza del guerrero. Los sayajines incrementan su fuerza en 2000, los demonios en 5000 y el resto en 100.
   a. Todos los guerreros se pueden transformar hasta 3 veces. Si un guerrero tiene 3 transformaciones realizadas y se intenta transformar, entonces queda igual.

5. Queremos saber **quién gana en un enfrentamiento** entre dos guerreros. Antes de pelear, ambos guerreros se transforman, en caso de que puedan hacerlo. El ganador será aquel que tenga mayor nivel de poder luego de esta transformación.

6. Modelar los **equipos**, que son conjuntos ordenados de guerreros, cuyo primer miembro se lo considera el líder.
   a. Modelar los Guerreros Z, que son un equipo compuesto por Goku, Yamcha y Piccolo.
   b. Modelar las Fuerzas de Freezer, que están compuestos por Freezer y Dodoria.

7. Queremos poder **agregar una persona a un equipo**. Para que esto pueda suceder, la persona debe tener por lo menos la mitad del nivel de fuerza del líder. Si no cumple este requisito, el equipo queda igual. Si puede sumarse al equipo, se lo agrega al final.

8. Hacer los *tests* que sean necesarios.