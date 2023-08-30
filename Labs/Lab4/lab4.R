library(tidyverse)
library(highcharter)
library(readr)
library(dplyr)
library(readxl)
library(ggplot2)
setwd('/Users/josuer/Documents/Sexto Semestre/Data Wrangling/Lab4')


#Lectura de datos
data = read_xlsx('lab4_datos.xlsx')
#Limpieza de datos
data$Estado <- "Despacho a Cliente|Despacho a cliente"
data$Estado <- ifelse(str_detect(data$CLIENTE, "\\|\\|\\|Faltante|FALTANTE"), "Faltante", data$Estado)
data$Estado <- ifelse(str_detect(data$CLIENTE, "\\|\\|\\|DEVOLUCION"), "Devolución", data$Estado)

conteo_devoluciones <- sum(data$Estado == "Devolución")
conteo_devoluciones

clientes_mas_pedidos <- data %>%
  group_by(CLIENTE, Estado) %>%
  summarise(Cantidad_Pedidos = n()) %>%
  arrange(desc(Cantidad_Pedidos))
clientes_mas_pedidos


# ENVIOS X PILOTO
envios_por_piloto <- table(data$PILOTO)
envios_por_piloto_df <- as.data.frame(envios_por_piloto)
colnames(envios_por_piloto_df) <- c("Piloto", "Envios")
print(envios_por_piloto_df) 

envios = data %>% 
  group_by(PILOTO) %>% 
  summarise(Envios = n()) %>% 
  hchart('column', hcaes(x=PILOTO, y=Envios)) %>% 
  hc_title(text = "<b>Total de envios por piloto</b>") %>% 
  hc_subtitle(text = "<i>El mejor piloto fue Fernando con 267 envios</i>")
envios

#Envios por mes y por tipo de estado
envios_por_estado <- data %>%
  group_by(Fecha, Estado) %>%
  summarise(Envios = n()) 
ggplot(envios_por_estado, aes(fill=Estado, y=Envios, x=Fecha))+
  geom_bar(position = 'stack', stat = 'identity')+ 
  geom_text(aes(label = Envios))+
  xlab("Fecha")+
  ylab("Estado")+
  ggtitle("Cantidad de viajes por mes y tipo de estado", subtitle = "El mes con mas envios fue mayo con 215 envios, 135 fueron despachos exitosos")
print(envios_por_estado, n=Inf)


#ENVIOS X MES
envios_x_mes = data %>% 
  group_by(Fecha) %>% 
  summarise(Envios = n()) %>% 
  hchart('column', hcaes(x = Fecha, y = Envios), name = "Cantidad de envíos") %>%
  hc_title(text = "<b>Cantidad de envíos por mes</b>") %>% 
  hc_subtitle(text = "<i>El mes que presento mas envios fue mayo con 215</i>")
print(envios_x_mes)



# Envios por tipo de camion
pedidos_por_tipo_de_camion <- data %>%
  group_by(UNIDAD) %>%
  summarise(Pedidos = n()) %>% 
  hchart('bar', hcaes(x=UNIDAD, y=Pedidos), name="Cantidad de envios por unidad") %>% 
  hc_title(text="<br>Envios segun tipo de camion</b>") %>% 
  hc_subtitle(text="<i>El camion grande realizo el mayor numero de entregas y panel el menor</i>") %>% 
  hc_labels()
print(pedidos_por_tipo_de_camion, n=Inf)

#PEDIDOS POR TIPO DE CAMION 
pedidos_por_tipo_de_camion <- pedidos_por_tipo_de_camion %>%
  arrange((Pedidos)) %>% 
  hchart('bar', hcaes(x=Pedidos, y=UNIDAD), NAME)
print(pedidos_por_tipo_de_camion, n = Inf)

#Ganancias por camion
ganancias_por_camion <- data %>% 
  group_by(UNIDAD) %>% 
  summarise(Ingreso=sum(Q)) %>% 
  hchart('bar', hcaes(x=UNIDAD, y=Ingreso), name="Ingresos por unidad") %>% 
  hc_title(text="<b>Ingresos por unidad</b>")
ganancias_por_camion


#Dias de credito
promedio_dias_credito <- mean(data$CREDITO)
print(promedio_dias_credito)

#PEDIDOS POR MES DEL QUE MAS HIZO
pedidos_por_piloto_por_mes <- data %>%
  group_by('Fernando Mariano Berrio', Fecha) %>%
  summarise(Cantidad_Pedidos = n()) %>% 
  hchart('column', hcaes(x=Fecha, y=Cantidad_Pedidos), name = 'Mejor Piloto') %>% 
  hc_title(text = "<b>Mejor piloto</b>")
print(pedidos_por_piloto_por_mes,  n = Inf)

#CANTIDAD DE PEDIDOS POR CLIENTE
top_clientes <- data %>%
  group_by(CLIENTE) %>%
  summarise(Cantidad_Pedidos = n()) %>%
  arrange(desc(Cantidad_Pedidos)) %>%
  head(5) %>% 
  
top_clientes

#CANTIDAD COMPRADA POR CLIENTE (Despacho Exitoso)
cantidad_comprada_top_5 <- data %>%  
  group_by(CLIENTE) %>% 
  filter(CLIENTE %in% c("EL PINCHE OBELISCO / Despacho a cliente", 
                        "TAQUERIA EL CHINITO", 
                        "UBIQUO LABS", 
                        "UNIVERSIDAD FRANCISCO MARROQUIN/Despacho a cliente",
                        "ABARROTERIA EBENEZER/Despacho a cliente")) %>% 
  summarise_at(vars(Q), list(Q=sum)) %>% 
  arrange((-Q)) %>% 
  hchart('column', hcaes(x=CLIENTE, y=Q)) %>% 
  hc_title(text="<b>Top 5 clientes que mas compraron</b>") %>% 
  hc_subtitle(text="<i>Despachos exitosos</i>")
cantidad_comprada_top_5


#CANTIDAD COMPRADA POR CLIENTE (Faltantes)
cantidad_comprada_top_4 <- data %>%  
  group_by(CLIENTE) %>% 
  filter(CLIENTE %in% c("EL PINCHE OBELISCO |||Faltante", 
                        "TAQUERIA EL CHINITO |||Faltante", 
                        "UBIQUO LABS |||FALTANTE", 
                        "POLLO PINULITO|||FALTANTE")) %>% 
  summarise_at(vars(Q), list(Q=sum)) %>% 
  arrange((-Q)) %>% 
  hchart('column', hcaes(x=CLIENTE, y=Q)) %>% 
  hc_title(text="<b>Top 4 clientes que mas compraron</b>") %>% 
  hc_subtitle(text="<i>Despachos que faltaban</i>")
cantidad_comprada_top_4

#CANTIDAD Devuelta (Faltantes)
cantidad_devuelta <- data %>%  
  group_by(CLIENTE) %>% 
  filter(CLIENTE %in% c("EL GALLO NEGRO |||DEVOLUCION")) %>% 
  summarise_at(vars(Q), list(Q=sum)) %>% 
  arrange((-Q)) %>% 
  hchart('column', hcaes(x=CLIENTE, y=Q)) %>% 
  hc_title(text="<b>Devoluciones</b>") %>% 
  hc_subtitle(text="<i>Cantidad que el cliente devolvio Q33,070.50</i>")
cantidad_devuelta



