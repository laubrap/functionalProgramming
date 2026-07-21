## The Big Bang Haskell - Orden Superior

En Pasadena, California, vive un grupo peculiar de amigos. Queremos modelar a cada persona. De cada uno sabemos que tienen un nombre, un nivel de ansiedad, un grado académico, y algunos intereses.

1. Modelar a los siguientes personajes, sabiendo que el orden de los grados de menor a mayor es Ninguno, Ingeniería, Doctorado, Nobel.
   - **Sheldon:** su nivel de ansiedad es de 30, su grado es Nobel, y sus intereses son Star Trek, la física, los videojuegos y los trenes
   - **Leonard:** su nivel de ansiedad es 80, su grado es Doctorado y está interesado en los videojuegos y los comics
   - **Howard:** le gusta la magia, los cinturones y los cohetes, su nivel de ansiedad es 90 y su grado es Ingenieria
   - **Amy:** su nivel de ansiedad es 50, le gusta la neurobiología, el arpa y su grado es Doctorado
   - **Penny:** no tiene ningún título, por lo que su grado académico es Ninguno, está interesada en la actuación y el maquillaje, y su nivel de ansiedad es de 20 puntos.

2. Podemos agrupar a las personas. Contamos con los siguientes **grupos**:
   - **departamento4A:** conformado por Leonard y Sheldon
   - **vecinos:** compuesto por Leonard y Penny

3. Queremos saber si todos los miembros de un grupo **son respetables.** Lo cual ocurre cuando su grado académico es mayor que Ingeniería.

4. Hacer que un grupo pueda **realizar una actividad**, que implica que todos los del grupo realicen dicha actividad
   - **Noche de Halo:** es decir que todos hayan jugado a Halo. Cuando una persona juega, se disminuye 10 puntos su nivel de ansiedad y se agregan los videojuegos a sus intereses (a menos que ya lo tuviera, en ese caso no se modifican)
   - **Competencia de paintball:** se sabe que esta actividad es estresante. Aumenta en 20 puntos el nivel de ansiedad de cada integrante y agrega el paintball a su lista de intereses (a menos que ya lo tuviera).

5. Es regla tomar una bebida caliente para calmar la ansiedad. Buscamos a los que **necesitan un té** de cada grupo. Una persona necesita tomarse un té cuando está muy ansioso, que es cuando su nivel de ansiedad es mayor a 80 puntos.

6. Saber si **hay algún obsesivo** en el grupo, lo cual ocurre cuando tiene 3 o más intereses.

7. Queremos saber si un grupo está en **crisis total.** Esto ocurre cuando la suma de sus niveles de ansiedad es mayor a 300.

8. Lo mandaron a Sheldon al Polo Norte!! Además lo dejan ir acompañado. Decidió armar un conjunto de grupos (una delegación científica) para que cada uno se especialice en distintas tareas. Dada una delegación, queremos quedarnos solo con **los grupos aptos** para ir a la expedición. Para saber si cada grupo es apto, se tienen que cumplir dos condiciones:
   - Tiene que ser respetable (todos sus miembros son respetables).
   - Su **potencial de estrés** tiene que ser bajo (menor a 200). El potencial de estrés es la suma de los niveles de ansiedad de los miembros del grupo **LUEGO** de que este haya participado de una competencia de Paintball.

9. Además de la expedición al Polo Norte, sabemos que las delegaciones científicas pueden ir a otras expediciones varias. De cada expedición sabemos el nombre, su delegación asignada y un evento principal que puede suceder en ella.

   Los **eventos** son situaciones adversas que enfrentan las delegaciones. Conocemos algunos eventos:
   - **Tormenta de nieve:** aumenta la ansiedad de todos los grupos de la delegación en 20 puntos
   - Modelar un evento más, ¿qué tipo debería ser?

10. Sheldon cree que el verdadero peligro en el Polo Norte es que la gente se vuelva loca. Por eso, el **nivel de peligrosidad** de una expedición es la cantidad total de personas que están próximas a volverse locas, que son aquellos que necesitan un té en toda la delegación (luego de que ocurra el evento).

11. Una expedición **se debe cancelar** cuando su nivel de peligrosidad es mayor a 2 o si algún grupo de la delegación está en crisis total.