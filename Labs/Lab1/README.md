README
================
2023-08-07

# PROBLEMA 1

``` r
library(readxl)
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
archivo <- "datos_unificados_con_fecha.xlsx"
datos <- read_excel(archivo)

head(datos)
```

    ## # A tibble: 6 × 8
    ##   COD_VIAJE CLIENTE                   CANTIDAD PILOTO     Q CREDITO UNIDAD Fecha
    ##       <dbl> <chr>                        <dbl> <chr>  <dbl>   <dbl> <chr>  <chr>
    ## 1  10000001 EL PINCHE OBELISCO / Des…     1200 Ferna… 300        30 Camio… 01-2…
    ## 2  10000002 TAQUERIA EL CHINITO |||F…     1433 Hecto… 358.       90 Camio… 01-2…
    ## 3  10000003 TIENDA LA BENDICION / De…     1857 Pedro… 464.       60 Camio… 01-2…
    ## 4  10000004 TAQUERIA EL CHINITO            339 Angel…  84.8      30 Panel  01-2…
    ## 5  10000005 CHICHARRONERIA EL RICO C…     1644 Juan … 411        30 Camio… 01-2…
    ## 6  10000006 UBIQUO LABS |||FALTANTE       1827 Luis … 457.       30 Camio… 01-2…

``` r
summary(datos)
```

    ##    COD_VIAJE       CLIENTE             CANTIDAD         PILOTO         
    ##  Min.   :1e+07   Length:2180        Min.   : 200.0   Length:2180       
    ##  1st Qu.:1e+07   Class :character   1st Qu.: 642.5   Class :character  
    ##  Median :1e+07   Mode  :character   Median :1097.5   Mode  :character  
    ##  Mean   :1e+07                      Mean   :1098.8                     
    ##  3rd Qu.:1e+07                      3rd Qu.:1562.0                     
    ##  Max.   :1e+07                      Max.   :1996.0                     
    ##        Q            CREDITO         UNIDAD             Fecha          
    ##  Min.   : 50.0   Min.   :30.00   Length:2180        Length:2180       
    ##  1st Qu.:160.6   1st Qu.:30.00   Class :character   Class :character  
    ##  Median :274.4   Median :60.00   Mode  :character   Mode  :character  
    ##  Mean   :274.7   Mean   :58.49                                        
    ##  3rd Qu.:390.5   3rd Qu.:90.00                                        
    ##  Max.   :499.0   Max.   :90.00

# PROBLEMA 2

``` r
lista_vectores <- list(
  c(23,123,23,42,123,34,23),
  c(212,56,34,56,32,34,56),
  c(11,3,22,11,53,23,34)
)


calcular_moda <- function(x) {
  tabla_frec <- table(x)
  moda <- as.numeric(names(tabla_frec)[tabla_frec == max(tabla_frec)])
  return(moda)
}


modas <- lapply(lista_vectores, calcular_moda)
print(modas)
```

    ## [[1]]
    ## [1] 23
    ## 
    ## [[2]]
    ## [1] 56
    ## 
    ## [[3]]
    ## [1] 11

# PROBLEMA 3

``` r
library(readr)

suppressWarnings({
datos_SAT = read_delim("/Users/josuer/Documents/Sexto Semestre/Data Wrangling/INE_PARQUE_VEHICULAR_2019.txt", delim="|")
})
```

    ## New names:
    ## Rows: 2435294 Columns: 11
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "|" chr
    ## (8): MES, NOMBRE_DEPARTAMENTO, NOMBRE_MUNICIPIO, MODELO_VEHICULO, LINEA_... dbl
    ## (2): ANIO_ALZA, CANTIDAD lgl (1): ...11
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...11`

``` r
head(datos_SAT)
```

    ## # A tibble: 6 × 11
    ##   ANIO_ALZA MES   NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO MODELO_VEHICULO
    ##       <dbl> <chr> <chr>               <chr>            <chr>          
    ## 1      2007 05    HUEHUETENANGO       "HUEHUETENANGO"  2007           
    ## 2      2007 05    EL PROGRESO         "EL JICARO"      2007           
    ## 3      2007 05    SAN MARCOS          "OCOS"           2007           
    ## 4      2007 05    ESCUINTLA           "SAN JOS\xc9"    2006           
    ## 5      2007 05    JUTIAPA             "MOYUTA"         2007           
    ## 6      2007 05    GUATEMALA           "FRAIJANES"      1997           
    ## # ℹ 6 more variables: LINEA_VEHICULO <chr>, TIPO_VEHICULO <chr>,
    ## #   USO_VEHICULO <chr>, MARCA_VEHICULO <chr>, CANTIDAD <dbl>, ...11 <lgl>

``` r
summary(datos_SAT)
```

    ##    ANIO_ALZA        MES            NOMBRE_DEPARTAMENTO NOMBRE_MUNICIPIO  
    ##  Min.   :1980   Length:2435294     Length:2435294      Length:2435294    
    ##  1st Qu.:2006   Class :character   Class :character    Class :character  
    ##  Median :2011   Mode  :character   Mode  :character    Mode  :character  
    ##  Mean   :2010                                                            
    ##  3rd Qu.:2016                                                            
    ##  Max.   :2019                                                            
    ##  MODELO_VEHICULO    LINEA_VEHICULO     TIPO_VEHICULO      USO_VEHICULO      
    ##  Length:2435294     Length:2435294     Length:2435294     Length:2435294    
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  MARCA_VEHICULO        CANTIDAD        ...11        
    ##  Length:2435294     Min.   :  1.000   Mode:logical  
    ##  Class :character   1st Qu.:  1.000   NA's:2435294  
    ##  Mode  :character   Median :  1.000                 
    ##                     Mean   :  1.449                 
    ##                     3rd Qu.:  1.000                 
    ##                     Max.   :509.000
