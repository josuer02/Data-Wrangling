Lab 5
================
Josuer
2023-10-01

## Parte 1 Eclipse

``` r
eclipse = mdy_hms("August 21 2017 18:26:40")
synodic_months = days(29) + hours(12) + minutes(44) + seconds(3)
saros_time = 223 * synodic_months
nuevo_eclipse = eclipse + saros_time
paste0("El proximo eclipse sera en: ", nuevo_eclipse)
```

    ## [1] "El proximo eclipse sera en: 2035-09-02 02:09:49"

## Parte 2 Agrupaciones y operaciones con fechas

``` r
names(data) = tolower(names(data))
names(data) = gsub("ó", "o", names(data))
names(data) = gsub(" ", "_", names(data))

data <- data %>%
    mutate(fecha_corregida = as.Date(as.numeric(fecha_creacion), origin ="1899-12-30"))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `fecha_corregida = as.Date(as.numeric(fecha_creacion), origin =
    ##   "1899-12-30")`.
    ## Caused by warning in `as.Date()`:
    ## ! NAs introduced by coercion

``` r
data <- data %>%
    mutate(fecha_final_C = dmy(fecha_creacion))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `fecha_final_C = dmy(fecha_creacion)`.
    ## Caused by warning:
    ## !  104237 failed to parse.

``` r
data <- data %>%
    mutate(fecha = paste(fecha_corregida,fecha_final_C,sep=""))
data <- data %>%
    mutate(fecha = gsub("NA","",fecha))
data <- data %>%
    mutate(fecha = ymd(fecha))
data <- data %>%
    mutate(fecha_creacion = make_datetime(year(fecha),
                                      month(fecha),
                                      day(fecha),
                                      hour(hora_creacion),
                                      minute(hora_creacion), 
                                      second(hora_creacion)))

data <- data %>%
  select(-c("fecha", "fecha_corregida", "fecha_final_C", "hora_creacion"))

data <- data %>%
    mutate(fecha_corregida = as.Date(as.numeric(fecha_final), origin ="1899-12-30"))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `fecha_corregida = as.Date(as.numeric(fecha_final), origin =
    ##   "1899-12-30")`.
    ## Caused by warning in `as.Date()`:
    ## ! NAs introduced by coercion

``` r
data <- data %>%
    mutate(fecha_final_C = dmy(fecha_final))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `fecha_final_C = dmy(fecha_final)`.
    ## Caused by warning:
    ## !  104237 failed to parse.

``` r
data <- data %>%
    mutate(fecha = paste(fecha_corregida,fecha_final_C,sep=""))
data <- data %>%
    mutate(fecha = gsub("NA","",fecha))
data <- data %>%
    mutate(fecha = ymd(fecha))
data <- data %>%
    mutate(dia = day(hora_final))
data <- data %>% 
  mutate(fecha = case_when(dia == 1 ~ fecha + ddays(x=1),
                           dia == 31 ~ fecha))
data <- data %>%
    mutate(fecha_final = make_datetime(year(fecha),
                                      month(fecha),
                                      day(fecha),
                                      hour(hora_final),
                                      minute(hora_final), 
                                      second(hora_final)))
data <- data %>%
  select(-c("fecha", "fecha_corregida", "fecha_final_C", "hora_final", "dia"))
```

### ¿En qué meses existe una mayor cantidad de llamadas por código?

``` r
mayor_cantidad = data %>% 
  mutate(mes = month(fecha_creacion)) %>% 
  filter(call == 1) %>% 
  select(mes, cod) %>% 
  group_by(mes, cod) %>% 
  summarise(cantidad_llamadas = n()) %>% 
  arrange(desc(cantidad_llamadas))
```

    ## `summarise()` has grouped output by 'mes'. You can override using the `.groups`
    ## argument.

``` r
mayor_cantidad
```

    ## # A tibble: 12 × 3
    ## # Groups:   mes [12]
    ##      mes cod                          cantidad_llamadas
    ##    <dbl> <chr>                                    <int>
    ##  1     3 Actualización de Información               497
    ##  2     7 Actualización de Información               496
    ##  3     5 Actualización de Información               494
    ##  4    11 Actualización de Información               493
    ##  5    10 Actualización de Información               487
    ##  6    12 Actualización de Información               478
    ##  7     8 Actualización de Información               474
    ##  8     6 Actualización de Información               471
    ##  9     1 Actualización de Información               465
    ## 10     9 Actualización de Información               465
    ## 11     4 Actualización de Información               462
    ## 12     2 Actualización de Información               443

### ¿Qué día de la semana es el más ocupado?

``` r
dia_ocupado <- data %>% 
  mutate(Dia = wday(fecha_creacion, label = TRUE, abbr = FALSE)) %>% 
  group_by(Dia) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad)) %>% 
  head(1)
dia_ocupado
```

    ## # A tibble: 1 × 2
    ##   Dia    cantidad
    ##   <ord>     <int>
    ## 1 Sunday    38254

### ¿Qué mes es el más ocupado?

``` r
mes_ocupado <- data %>% 
  mutate(Mes = month(fecha_creacion)) %>% 
  group_by(Mes) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad)) %>% 
  head(1)
mes_ocupado
```

    ## # A tibble: 1 × 2
    ##     Mes cantidad
    ##   <dbl>    <int>
    ## 1     3    22708

### ¿Existe una concentración o estacionalidad en la cantidad de llamadas?

``` r
estacionalidad <- data %>% 
  filter(call == 1) %>% 
  mutate(Semana = floor_date(fecha_creacion, unit = "week")) %>% 
  group_by(Semana) %>% 
  summarise(conteo = n())
estacionalidad = data.frame(estacionalidad)

#Test de estacionalidad
kpss.test(estacionalidad[,2], null ="Trend") 
```

    ## Warning in kpss.test(estacionalidad[, 2], null = "Trend"): p-value greater than
    ## printed p-value

    ## 
    ##  KPSS Test for Trend Stationarity
    ## 
    ## data:  estacionalidad[, 2]
    ## KPSS Trend = 0.11412, Truncation lag parameter = 3, p-value = 0.1

``` r
print("Si hay una estacionalidad en la cantidad de llamadas")
```

    ## [1] "Si hay una estacionalidad en la cantidad de llamadas"

### ¿Cuántos minutos dura la llamada promedio?

``` r
duracion <- data %>% 
  filter(call == 1) %>% 
  summarise(Duracion_Promedio = mean(difftime(fecha_final, fecha_creacion, "minutes")))
  
dura = as.numeric(duracion$Duracion_Promedio, units = "mins")
paste0("Una llamada dura en promedio ", dura, " mins")
```

    ## [1] "Una llamada dura en promedio 14.557903930131 mins"

### Realice una tabla de frecuencias con el tiempo de llamada

``` r
tabla_freq <- data %>% 
  filter(call == 1) %>% 
  mutate(duracion = (fecha_final - fecha_creacion)) %>% 
  select(duracion)
tabla_freq$duracion = round(as.numeric(tabla_freq$duracion)/60)
tabla  <- as.data.frame(table(cut(tabla_freq$duracion,
                                  seq(min(tabla_freq$duracion),
                                      max(tabla_freq$duracion),
                                      5)
                                      ))) 
tabla <- tabla %>% 
  rename("Tiempo en minutos" = Var1, 
         "Cantidad de llamadas" = Freq)
tabla
```

    ##   Tiempo en minutos Cantidad de llamadas
    ## 1             (0,5]                  956
    ## 2            (5,10]                  959
    ## 3           (10,15]                  920
    ## 4           (15,20]                  914
    ## 5           (20,25]                  932
    ## 6           (25,30]                  823

## Parte 3 Signo Zodiacal

``` r
obtener_signo_zodiacal <- function(fecha_nacimiento) {
  mes <- month(fecha_nacimiento)
  dia <- day(fecha_nacimiento)
  
  fechas_signos <- data.frame(
    signo = c("Aries", "Tauro", "Géminis", "Cáncer", "Leo", "Virgo", "Libra", "Escorpio", "Sagitario", "Capricornio", "Acuario", "Piscis"),
    inicio = ymd(c("2023-03-21", "2023-04-20", "2023-05-21", "2023-06-21", "2023-07-23", "2023-08-23", "2023-09-23", "2023-10-23", "2023-11-22", "2023-12-22", "2023-01-20", "2023-02-19")),
    fin = ymd(c("2023-04-19", "2023-05-20", "2023-06-20", "2023-07-22", "2023-08-22", "2023-09-22", "2023-10-22", "2023-11-21", "2023-12-21", "2023-01-19", "2023-02-18", "2023-03-20"))
  )
  
  signo_zodiacal <- fechas_signos$signo[mes == month(fechas_signos$inicio) & dia >= day(fechas_signos$inicio) | mes == month(fechas_signos$fin) & dia <= day(fechas_signos$fin)]
  
  if (length(signo_zodiacal) == 0) {
    return("Fecha de nacimiento no válida.")
  } else {
    return(signo_zodiacal)
  }
}

# Ejemplo:
fecha_nacimiento <- ymd("2002-11-01")
signo <- obtener_signo_zodiacal(fecha_nacimiento)
cat("Tu signo zodiacal es:", signo, "\n")
```

    ## Tu signo zodiacal es: Escorpio

## Parte 4 Flights

``` r
flights$dep_datetime <- with(flights, ymd_hm(sprintf("%04d%02d%02d%02d%02d", year, month, day, dep_time %/% 100, dep_time %% 100)))
```

    ## Warning: 8255 failed to parse.

``` r
flights$arr_datetime <- with(flights, ymd_hm(sprintf("%04d%02d%02d%02d%02d", year, month, day, arr_time %/% 100, arr_time %% 100)))
```

    ## Warning: 8713 failed to parse.

``` r
flights$sched_dep_datetime <- with(flights, ymd_hm(sprintf("%04d%02d%02d%02d%02d", year, month, day, sched_dep_time %/% 100, sched_dep_time %% 100)))
flights$sched_arr_datetime <- with(flights, ymd_hm(sprintf("%04d%02d%02d%02d%02d", year, month, day, sched_arr_time %/% 100, sched_arr_time %% 100)))

# Visualizar las nuevas columnas
head(flights[c("dep_datetime", "arr_datetime", "sched_dep_datetime", "sched_arr_datetime")])
```

    ## # A tibble: 6 × 4
    ##   dep_datetime        arr_datetime        sched_dep_datetime 
    ##   <dttm>              <dttm>              <dttm>             
    ## 1 2013-01-01 05:17:00 2013-01-01 08:30:00 2013-01-01 05:15:00
    ## 2 2013-01-01 05:33:00 2013-01-01 08:50:00 2013-01-01 05:29:00
    ## 3 2013-01-01 05:42:00 2013-01-01 09:23:00 2013-01-01 05:40:00
    ## 4 2013-01-01 05:44:00 2013-01-01 10:04:00 2013-01-01 05:45:00
    ## 5 2013-01-01 05:54:00 2013-01-01 08:12:00 2013-01-01 06:00:00
    ## 6 2013-01-01 05:54:00 2013-01-01 07:40:00 2013-01-01 05:58:00
    ## # ℹ 1 more variable: sched_arr_datetime <dttm>

``` r
flights <- flights %>%
  mutate(
    delay_total = (dep_datetime - sched_dep_datetime) + (arr_datetime - sched_arr_datetime)
  )

# Visualizar las primeras filas de la columna delay_total
head(flights %>% select(dep_datetime, arr_datetime, sched_dep_datetime, sched_arr_datetime, delay_total))
```

    ## # A tibble: 6 × 5
    ##   dep_datetime        arr_datetime        sched_dep_datetime 
    ##   <dttm>              <dttm>              <dttm>             
    ## 1 2013-01-01 05:17:00 2013-01-01 08:30:00 2013-01-01 05:15:00
    ## 2 2013-01-01 05:33:00 2013-01-01 08:50:00 2013-01-01 05:29:00
    ## 3 2013-01-01 05:42:00 2013-01-01 09:23:00 2013-01-01 05:40:00
    ## 4 2013-01-01 05:44:00 2013-01-01 10:04:00 2013-01-01 05:45:00
    ## 5 2013-01-01 05:54:00 2013-01-01 08:12:00 2013-01-01 06:00:00
    ## 6 2013-01-01 05:54:00 2013-01-01 07:40:00 2013-01-01 05:58:00
    ## # ℹ 2 more variables: sched_arr_datetime <dttm>, delay_total <drtn>
