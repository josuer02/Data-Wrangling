dw-2023-parcial-1
================
Tepi
9/11/2023

# Examen parcial

Indicaciones generales:

- Usted tiene el período de la clase para resolver el examen parcial.

- La entrega del parcial, al igual que las tareas, es por medio de su
  cuenta de github, pegando el link en el portal de MiU.

- Pueden hacer uso del material del curso e internet (stackoverflow,
  etc.). Sin embargo, si encontramos algún indicio de copia, se anulará
  el exámen para los estudiantes involucrados. Por lo tanto, aconsejamos
  no compartir las agregaciones que generen.

## Sección 0: Preguntas de temas vistos en clase (20pts)

- Responda las siguientes preguntas de temas que fueron tocados en
  clase.

1.  ¿Qué es una ufunc y por qué debemos de utilizarlas cuando
    programamos trabajando datos?

    - Las ufuc son funciones universales que aprovechan la vectorizacion
      para ser mas eficientes asi como otras operaciones matematicas, la
      vectorizacion ayuda ya que puede realizar operaciones dentro de
      arreglos enteros en lugar de estar individualmente.

2.  Es una técnica en programación numérica que amplía los objetos que
    son de menor dimensión para que sean compatibles con los de mayor
    dimensión. Describa cuál es la técnica y de un breve ejemplo en R.

    - Esta tecnica es conocida como Broadcasting que permite realizar
      operaciones entre arreglos de distintos tamaños sin tener que
      copiar o modificar lo datos.
    - Ejemplo:
      - `matriz <- matrix(1:12, nrow = 3)`
      - `vector <- c(10, 20, 30)`
      - `resultado <- matriz + vector`
      - En este ejemplo el vector se expande a una matriz para poder
        realizar la operacion de suma.

3.  ¿Qué es el axioma de elegibilidad y por qué es útil al momento de
    hacer análisis de datos? \*El axioma de elegibilidad es util en el
    analisis de datos para garantizar la relevancia y validez de los
    resultados al seleccionar casos que sean apropiados para el problema
    o la pregunta de investigación

4.  Cuál es la relación entre la granularidad y la agregación de datos?
    Mencione un breve ejemplo. Luego, explique cuál es la granularidad o
    agregación requerida para poder generar un reporte como el
    siguiente:

    - La granularidad se refiere a que tan especificos estan nuestros
      datos y la agregacion se dedica a combinar los datos a un nivel
      superior
    - Ejemplo: En una academia de futbol donde se tienen varios
      jugadores se puede hacer una granularidad por jugador y luego una
      agragacion por tipo de posicion que juega para poder ver cuantos
      jugadores juegan que posicion
    - Para generar un reporte como el de abajo se necesita de tener una
      granularidad de usuario y pais para posteriormente poder realizar
      una agregacion que seria una suma de los usuarios a nivel de pais.

| Pais | Usuarios |
|------|----------|
| US   | 1,934    |
| UK   | 2,133    |
| DE   | 1,234    |
| FR   | 4,332    |
| ROW  | 943      |

## Sección I: Preguntas teóricas. (50pts)

- Existen 10 preguntas directas en este Rmarkdown, de las cuales usted
  deberá responder 5. Las 5 a responder estarán determinadas por un
  muestreo aleatorio basado en su número de carné.

- Ingrese su número de carné en `set.seed()` y corra el chunk de R para
  determinar cuáles preguntas debe responder.

``` r
set.seed("20210418") 
v<- 1:10
preguntas <-sort(sample(v, size = 5, replace = FALSE ))

paste0("Mis preguntas a resolver son: ",paste0(preguntas,collapse = ", "))
```

    ## [1] "Mis preguntas a resolver son: 1, 3, 5, 9, 10"

### Listado de preguntas teóricas (Preguntas 1, 3, 5, 9, 10)

1.  Para las siguientes sentencias de `base R`, liste su contraparte de
    `dplyr`:

    - `str()`
    - `df[,c("a","b")]`
    - `names(df)[4] <- "new_name"` donde la posición 4 corresponde a la
      variable `old_name`
    - `df[df$variable == "valor",]`

    Contraparte de dplyr

    - `glimpse()`
    - `df %>% select(a, b)`
    - `df <- df %> rename(old_name = ‘new_name’)`
    - `df %>% filter(varible == “valor”)`

2.  ¿Por qué en R utilizamos funciones de la familia apply
    (lapply,vapply) en lugar de utilizar ciclos?

    - Porque aprovechan las ventajas de la vectorización, son más
      eficientes, legibles y propensas a errores que los ciclos
      tradicionales.

3.  ¿Cuál es la forma correcta de cargar un archivo de texto donde el
    delimitador es `:`?

    - La forma correcta de leer un archivo de texto con delimitador es
      usando read_delim, ya que esta nos permite enviar como parametro
      el nombre del archivo y el delimitador que en este caso es ‘:’.
      read_delim(data.txt, delim”:“)

4.  En SQL, ¿para qué utilizamos el keyword `HAVING`?

    - Utilizamos el keyword HAVING ya que al momento de realizar
      agregaciones no podemos usar el keyword WHERE. De esta manera
      podemos realizar los filtros que necesitemos tras realizar
      agregaciones.

5.  Si quiero obtener como resultado las filas de la tabla A que no se
    encuentran en la tabla B, ¿cómo debería de completar la siguiente
    sentencia de SQL?

    - SELECT \* FROM A LEFT JOIN B ON A.KEY = B.KEY WHERE B.KEY = NULL

Extra: ¿Cuántos posibles exámenes de 5 preguntas se pueden realizar
utilizando como banco las diez acá presentadas? (responder con código de
R.)

``` r
resultado = factorial(10)/factorial(5)
print('Los posibles examenes de 5 preguntas pueden ser: ')
```

    ## [1] "Los posibles examenes de 5 preguntas pueden ser: "

``` r
print(resultado)
```

    ## [1] 30240

``` r
#Resultado es de 30240
```

## Sección II Preguntas prácticas. (30pts)

- Conteste las siguientes preguntas utilizando sus conocimientos de R.
  Adjunte el código que utilizó para llegar a sus conclusiones en un
  chunk del markdown.

A. De los clientes que están en más de un país,¿cuál cree que es el más
rentable y por qué?

B. Estrategia de negocio ha decidido que ya no operará en aquellos
territorios cuyas pérdidas sean “considerables”. Bajo su criterio,
¿cuáles son estos territorios y por qué ya no debemos operar ahí?

### I. Preguntas teóricas

## A

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
parcial_anonimo = readRDS("parcial_anonimo.rds")

parcial_anonimo %>% 
  select(Cliente, Pais, Venta) %>% 
  group_by(Cliente) %>% 
  summarize(ventas=sum(Venta), Paises=n_distinct(Pais)) %>% 
  filter(Paises>1) %>% 
  arrange(desc(ventas)) %>% 
  head(1)
```

    ## # A tibble: 1 × 3
    ##   Cliente  ventas Paises
    ##   <chr>     <dbl>  <int>
    ## 1 a17a7558 19818.      2

``` r
print("El cliente mas rentable es el a17a7558 ya que es el que mas compra y esta en 2 paises.")
```

    ## [1] "El cliente mas rentable es el a17a7558 ya que es el que mas compra y esta en 2 paises."

- El cliente mas rentable es el a17a7558 ya que es el que mas compra y
  esta en 2 paises.

## B

``` r
###resuelva acá
parcial_anonimo %>% 
  select(Territorio, Venta) %>% 
  group_by(Territorio) %>% 
  summarise(n=sum(Venta)) %>% 
  filter(n<600) %>% 
  arrange(desc(n)) %>% 
  mutate(Perdida=n-600)
```

    ## # A tibble: 8 × 3
    ##   Territorio     n Perdida
    ##   <chr>      <dbl>   <dbl>
    ## 1 4163fa3f   580.    -20.4
    ## 2 456278b8   493.   -107. 
    ## 3 0bfe69a0   384.   -216. 
    ## 4 e034e3c8   247.   -353. 
    ## 5 79428560   132    -468  
    ## 6 368301e2   121.   -479. 
    ## 7 13b223c9    49.9  -550. 
    ## 8 e6fd9da9    18.2  -582.

- Estos son los 8 territorios en los que ya no se debe operar ya que
  representan perdida, suponiendo que nuestro costo de operacion es
  de 600. Estos territorios representan perdida significativa.
