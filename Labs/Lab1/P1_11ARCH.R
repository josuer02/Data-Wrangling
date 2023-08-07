library(readxl)
library(writexl)
library(lubridate)
library(dplyr)
library(readr)

archivos <- c("01-2018.xlsx","02-2018.xlsx","03-2018.xlsx",
              "04-2018.xlsx","05-2018.xlsx","06-2018.xlsx",
              "07-2018.xlsx","08-2018.xlsx","09-2018.xlsx",
              "10-2018.xlsx","11-2018.xlsx")

datos_lista <- lapply(archivos, function(archivo) {
  datos <- read_excel(archivo)
  
  # Verificar y quitar columnas no requeridas
  columnas_requeridas <- c("COD_VIAJE", "CLIENTE", "UBICACIÓN", "CANTIDAD", "PILOTO", "Q", "CREDITO", "UNIDAD")
  columnas_extra <- setdiff(colnames(datos), columnas_requeridas)
  datos <- datos[, !colnames(datos) %in% columnas_extra]
  
  # Extraer el mes y el año del nombre del archivo
  mes <- as.numeric(substr(archivo, 1, 2))
  año <- as.numeric(substr(archivo, 4, 7))
  
  # Crear la columna de fecha con el formato "01-2018"
  fecha_archivo <- paste0(formatC(mes, width = 2, flag = "0"), "-", año)
  datos <- mutate(datos, Fecha = fecha_archivo)
  
  write_xlsx(datos, archivo)  # Sobrescribir el archivo con los datos modificados
  datos
})

datos_unificados <- bind_rows(datos_lista)

nuevo_archivo <- "datos_unificados_con_fecha.xlsx"
write_xlsx(datos_unificados, nuevo_archivo)