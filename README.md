# Practica 3

## Material Practica 3 

Variables aleatorias discretas y funciones de distribución:

- PDFs: [diapos](https://github.com/alejandro-isglobal/alejandro-isglobal.github.io/blob/master/slides/practica4.pdf)

- Videos: [Video 1](https://youtu.be/pcZk2jKFs1o), [Video 2](https://youtu.be/-OUtWd7-nQ8), [Video 3](https://youtu.be/HBa-d8Ungsw)

## Trabajo en clase

comandos:

- <code>sample()</code>
- <code>plot()</code>


[Pregunta 1](Problema1.rnw)

[Pregunta 2](Problema2.png)


# Ejercicios Regresión lineal
# Ejercicio 1

A un grupo de estudiantes se les mide su altura y su peso obteniéndose los siguientes resultados de altura: 

178, 181, 168, 183, 164, 181, 174, 176, 174, 176, 181, 168, 164, 174, 171, 

y los siguientes pesos: 

82, 89, 68, 91, 65, 80, 79, 81, 80, 79, 82, 69, 67, 80, 78. 

Realiza el diagrama de dispersión (configura la altura como la variable independiente y el peso como la dependiente). 

- ¿Qúe tipo de relación ves?

a) Relación lineal simple creciente 
b) Relación lineal simple decreciente
c) Relación exponencia simple creciente
d) Relación exponencial simple decreciente
e) Relación logarítmica simple creciente
f) Relación logarítmica simple decreciente

- Calcula el modelo lineal por el método de los mínimos cuadrados y define la recta de regresión. Escribe la recta de la forma $peso = m*altura + b$ reemplazando m y b por los valores calculados.

- Añade la recta de regresión resultante al diagrama de dispersión.

- ¿Cuál es el coeficiente de determinación ($R^2$)?

- ¿Cuál es el coeficiente de correlación ($R$)?

- Si llega un estudiante que mide 173cm, ¿Cuál será su peso según nuestro modelo?

- Al gráfico de dispersión inicial con la recta de regresión resultante añádale el punto que acabas de predecir

- Ahora si sabes que el estudiante nuevo pesa 72Kg, ¿cuál será su altura?

# Ejercicio 2

- Carga en R-studio el fichero anscombe.txt que contiene cuatro conjunto de datos: x1-y1 , x2-y2, x3-y3 y, x4-y4.

- ¿Cuántas observaciones tiene el conjunto de datos?

- Calcula el modelo lineal por el método de los mínimos cuadrados y define la recta de regresión $y = m*x + b$ para cada una de los conjuntos de datos x1-y1 , x2-y2, x3-y3 y, x4-y4. Complete la siguiente tabla (redondea a 2 cifras decimales):


| Conjunto de datos | $\hat{x}$ | $\hat{y}$ | $\sum_{i=1}^{11}(x-\hat{x})^2$ | $\sum_{i=1}^{11}(y-\hat{y})^2$ | $m$ | $b$  |   $R$  |  $R^2$ | Si $x=8.5$, $y = ?$ |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| I: (x1,y1)| - | - | - | - | - | - | - | - |-  |
| I: (x2,y2)| - | - | - | - | - | - | - | - |-  |
| I: (x3,y3)| - | - | - | - | - | - | - | - |-  |
| I: (x4,y4)| - | - | - | - | - | - | - | - |-  |

En general, ¿qué puedes decir de los coeficientes de correlación ($R$
) y de determinación ($R^2$)
)?

a)  Son iguales y altos en todos los casos 
b)  Son iguales y bajos en todos los casos
c) Son diferentes pero altos en todos los casos
d) Son diferentes pero bajos en todos los casos
e) Son diferentes, algunos bajos y otros altos

¿Cuál de los 4 modelos se ajusta mejor a los datos?

a) Caso I 
b) Caso II
c) Caso III
d) Caso IV
e) Ninguno
f) Los 4 por igual


Para cada uno de los conjuntos de datos: realiza el digrama de dispersión, agrega la recta de regresión y el punto en x=8.5
 y su predición. Con respecto a la pregunta anterior, ¿sigues pensando lo mismo?
 
 
¿Qué puedes decir de cada conjunto de datos?

para I: (x1,y1)

a) Relación lineal de primer orden 
b) Relación lineal de orden superior
c) Relación lineal de primer orden con un valor atípico
d) Relación no lineal con un valor atípico


para I: (x2,y2) 

a) Relación lineal de primer orden 
b) Relación lineal de orden superior
c) Relación lineal de primer orden con un valor atípico
d) Relación no lineal con un valor atípico

para I: (x3,y3) 

a) Relación lineal de primer orden 
b) Relación lineal de orden superior
c) Relación lineal de primer orden con un valor atípico
d) Relación no lineal con un valor atípico

para I: (x4,y4) 

a) Relación lineal de primer orden 
b) Relación lineal de orden superior
c) Relación lineal de primer orden con un valor atípico
d) Relación no lineal con un valor atípico


¿Cómo mejorarías cada modelo de regresión?


para I: (x1,y1) 

a) No haría nada, está bien como está (correcta)
b) Probaría un modelo lineal de orden o grado superior
c) Eliminaría el dato atípico y volvería a calcular la recta de regresión
d) Eliminaría el dato atípico y se ve claramente la no linealidad de los datos


para I: (x2,y2) 

a) No haría nada, está bien como está (correcta)
b) Probaría un modelo lineal de orden o grado superior
c) Eliminaría el dato atípico y volvería a calcular la recta de regresión
d) Eliminaría el dato atípico y se ve claramente la no linealidad de los datos


para I: (x3,y3) 

a) No haría nada, está bien como está (correcta)
b) Probaría un modelo lineal de orden o grado superior
c) Eliminaría el dato atípico y volvería a calcular la recta de regresión
d) Eliminaría el dato atípico y se ve claramente la no linealidad de los datos


para I: (x4,y4) 

a) No haría nada, está bien como está (correcta)
b) Probaría un modelo lineal de orden o grado superior
c) Eliminaría el dato atípico y volvería a calcular la recta de regresión
d) Eliminaría el dato atípico y se ve claramente la no linealidad de los datos

Finalmente, ¿Cuál de las siguientes conclusiones crees que es válida?

a) Es importante hacer una inspección visual de los datos antes de procesarlos
b) Los coeficientes de correlación y de determinación por si solos no siempre sirven como medida de evaluación de la c) exactitud de un modelo de regresión
d) Todas las anteriores (correcta)
e) Ninguna de las anteriores


# Ejercicio 3
Estudiando los incendios forestales, se deduce que puede existir una relación entre la cantidad de lluvia caída durante los meses de verano (en mm) y el número de incendios declarados. Recopilamos la información de los últimos diez años y obtenemos los siguientes datos:


lluvia: 97, 27, 93, 175, 38	, 192, 28, 182, 61, 77	
Incendios: 521, 863, 712, 163, 138, 811, 534, 442, 963, 313	


- A la vista de estos datos, ¿podríamos hacer una previsión de cuál será el número aproximado de incendios que se declararán con una lluvia de 120 mm?

- Si calculamos el modelo de regresión simple de primer orden, ¿qué concluimos del coeficiente de determinación?


# Ejercicio 4

La hidrólisis de un cierto ester tiene lugar en medio ácido según un proceso cinético de primer orden. Partiendo de una concentración inicial de 30 mM del ester, se han medido las concentraciones del mismo a diferentes tiempos obteniéndose los resultados registrados en el fichero ester.txt. ¿Cuál estimas que ha sido la concentración a los 70 segundos de comenzar el proceso? (R 0.9608968)

<code>Datos4 = read.table("ester.txt",header=TRUE, sep=";",dec=".")</code>



Ajusta el modelo lineal sobre el logaritmo de la concentración
