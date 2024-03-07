# Practica 3

## Material Practica 3 

Variables aleatorias discretas y funciones de distribución:

- PDFs: [diapos](https://github.com/alejandro-isglobal/alejandro-isglobal.github.io/blob/master/slides/practica4.pdf)

- Videos: [Video 1](https://youtu.be/pcZk2jKFs1o), [Video 2](https://youtu.be/-OUtWd7-nQ8), [Video 3](https://youtu.be/HBa-d8Ungsw)

## Trabajo en clase

comandos:

- <code>sample()</code>
- <code>plot()</code>
- <code>points()</code>
- <code>lines()</code>
- <code>function()</code>
- <code>sapply()</code>
- <code>dbinom()</code>
- <code>pbinom()</code>
- <code>qbinom()</code>


[Pregunta 1](Pregunta1.png)


# Ejercicio 1

Supongamos que una tienda de comestibles compra cada día 5 botellas de leche desnatada al precio mayorista de 1.20€ por botella y vende la leche al por menor a 1.65€ la botella. Al acabar el día, la leche no vendida se retira del estante y el tendero recibe un crédito del distribuidor equivalente a tres cuartos del precio al por mayor. Si la distribución de probabilidad de la variable aleatoria X
, que representa el número de botellas que se venden por día, es:


| x | f(x) |
|:--:|:--:|
| 0| 1/15|
| 1| 2/15|
| 2| 2/15|
| 3| 3/15|
| 4| 4/15|
| 5| 3/15|


- Representa gráficamente la función de probabilidad del número de botellas vendidas por día:

- Representa gráficamente la función de distribución del número de botellas vendidas por día


|   | valor |
|:--:|:--:|
| f(2)| respuesta|
| F(2)| respuesta|
| f(3.5)| respuesta|
| F(3.5)| respuesta|
| f(6)| respuesta|
| F(6)| respuesta|


- ¿Cuál es el valor esperado del número de botellas vendidas por día?

- ¿Cuál es la mediana del número de botellas vendidas por día? 

- ¿Cuál es la varianza del número de botellas vendidas por día? 

- Realiza 30 simulaciones del número de botellas de leche vendidas por día. (Utiliza como semilla el número 12)

- Representa gráficamente la frecuencia de los resultados obtenidos y compáralo con el diagrama de la función de probabilidad.

- ¿Cuál es el valor de la media de los resultados de las simulaciones?

- ¿Cuál es la varianza de los resultados de las simulaciones?


- Ahora realiza 10000 simulaciones (usando la misma semilla) y compara resultados

Definamos una nueva variable aleatoria Y
 que representa el beneficio por las ventas.

$Y=1.65X+34(1.20)(5−X)−(1.20)(5)=0.75X−1.5$

- ¿Cuál es el beneficio esperado? 
- ¿Cuál es la varianza del beneficio? 

- Realiza 10000 simulaciones del beneficio diario. (Utiliza como semilla el número 123). Representa gráficamente la frecuencia de los resultados obtenidos y compáralo con el diagrama de la función de probabilidad.

- ¿Cuál es el valor de la media de los resultados de las simulaciones?

- ¿Cuál es la varianza de los resultados de las simulaciones?

