library(dplyr)
library(highcharter)
library(lubridate)
library(tidyverse)
library(ggplot2)
data = read.csv("c1.csv")

#limpieza de datos
data <- data %>% 
  pivot_longer(c("Camion_5","Pickup","Moto"),
               names_to = "Vehiculo", values_to = "Costo")

data <- data %>% 
  pivot_longer(c("fijoCamion_5","fijoPickup","fijoMoto"),
               names_to = "Fijo", values_to = "CostoFijo")

data <- data %>% 
  pivot_longer(c("directoCamion_5","directoPickup","directoMoto"),
               names_to = "Directo", values_to = "CostoDirecto")


data = data %>% 
  mutate(
    Costo = gsub("Q-", "0", Costo),
    CostoFijo = gsub("Q-", "0", CostoFijo),
    CostoDirecto = gsub("Q-", "0", CostoDirecto)
  )

data = data[-c(9,10,11,12,13,14,15,16,17,18,19)]
data$factura <- str_remove(data$factura, "Q")
data$factura <- as.numeric(data$factura)
data$CostoDirecto<-str_remove(data$CostoDirecto, "Q")
data$CostoDirecto<-as.numeric(data$CostoDirecto)
data$CostoFijo<-str_remove(data$CostoFijo, "Q")
data$CostoFijo<-as.numeric(data$CostoFijo)
data$Costo<-str_remove(data$Costo, "Q")
data$Costo<-as.numeric(data$Costo)


centrosDistribucion <- data %>%
  count(origen) %>%
  mutate(n = format(n, big.mark = ","))

print(centrosDistribucion)

tipoOperacion <- data %>%
  count(Cod)
tipoOperacion

data$Ganancias <- data$factura - data$Costo
gananciaTotal <- sum(data$Ganancias)

data$Mes <- month(data$Fecha)
operacionesMes <- data %>%
  select(Mes, ID) %>%
  group_by(Mes)%>%
  summarise(operaciones = n()) %>%
  hchart("column", hcaes(x =  Mes, y = operaciones)) %>%
  hc_title(text = "<b>Cantidad de Operaciones por mes en el 2017</b>") %>%
  hc_subtitle(text = "<i>El mes mas recurrido de operaciones fue octubre. El mes menos recurrido fue febrero.</i>")
operacionesMes

# tipo de operaciones
tipo <- data %>%
  count(Cod) %>% 
  select(Cod, n)%>%
  hchart("column", hcaes(x =  Cod, y = n)) %>%
  hc_title(text = "<b>Cantidad de Operaciones por tipo de servicio.</b>") %>%
  hc_subtitle(text = "<i>El servicio que mas se realiza es Revision. El servicio que menos se realiza es Cambio de Puentes.</i>")
tipo

# facturacion mensual
facturames <- data %>%
  select(Mes, factura) %>%
  group_by(Mes) %>%
  summarise(facturacion = sum(factura))

ggplot(facturames, aes(y=facturacion, x=Mes)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  scale_y_continuous(breaks = seq(0, 3200000, by = 500000))+
  xlab("Mes")+
  ylab("Cantidad Facturada")+
  ggtitle("Cantidad facturada por mes operativo.")

# ganancias mensuales
gananciasxmes <- data %>%
  select(Mes, Ganancias) %>%
  group_by(Mes) %>%
  summarise(gananciasM = sum(Ganancias))

ggplot(gananciasxmes, aes(y=gananciasM, x=Mes)) + 
  geom_bar(position="stack", stat="identity")+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  scale_y_continuous(breaks = seq(0, 800000, by = 50000))+
  xlab("Mes")+
  ylab("Ganancias")+
  ggtitle("Ganancias por mes operativo.")

codVehiculo <- data %>%
  filter(Vehiculo == "Pickup") %>%
  group_by(Cod) %>%
  summarise(operaciones = n()) %>%
  mutate(Cod = as.factor(Cod))  # Convertir a factor

# Crear gráfico de barras horizontales
ggplot(codVehiculo, aes(x = operaciones, y = Cod, fill = Cod)) +
  geom_bar(stat = "identity", color = "black") +
  xlab("Cantidad de Operaciones") +
  ylab("Código") +
  ggtitle("Distribución de operaciones para pickup") +
  theme_minimal() +
  scale_x_continuous(labels = scales::comma)

#ganancias por vehiculo
gananciaVehiculo <- data %>%
  select(Vehiculo, Ganancias) %>%
  group_by(Vehiculo) %>%
  summarise(gananciasv = sum(Ganancias)) %>%
  hchart("column", hcaes(x =  Vehiculo, y = gananciasv)) %>%
  hc_title(text = "<b>Ganancias Segun medio de Transporte.</b>") %>%
  hc_subtitle(text = "<i>El tipo de vehiculo que mas ganancias aporta es la Moto</i>")
gananciaVehiculo

facturas <- data %>%
  select(factura, Cod) %>% 
  group_by(Cod) %>% 
  summarise(cantidad = n()) %>% 
  arrange(desc(cantidad)) %>%
  mutate(cantidad = format(cantidad, big.mark = ","))

print(facturas)


tarifa_costo_promedio <- data %>%
  group_by(Cod) %>%
  summarise(
    tarifa_promedio = mean(factura),
    costo_promedio = mean(Costo)
  )

tarifa_costo_promedio

tarifa_costo_promedio_vehiculo <- data %>%
  group_by(Vehiculo) %>%
  summarise(
    tarifa_promedio = mean(factura),
    costo_promedio = mean(CostoFijo)
  )

tarifa_costo_promedio_vehiculo

data <- data %>%
  mutate(margen_porcentaje = ((factura - Costo) / factura) * 100)

# Calcular el promedio de margen como porcentaje
promedio_margen_porcentaje <- mean(data$margen_porcentaje, na.rm = TRUE)

promedio_margen_porcentaje



Contabilidad <- data %>%
  summarise(totalFacturacion = sum(factura),
            totalCosto = sum(Costo),
            totalCostoD = sum(CostoDirecto),
            totalCostoF = sum(CostoFijo),
            totalGanancia = sum(Ganancias))

Contabilidad$MargenAnual <- Contabilidad$totalGanancia/Contabilidad$totalFacturacion
Contabilidad$MargenAnual <- formattable::percent(Contabilidad$MargenAnual)

costo_total_por_vehiculo <- data %>%
  group_by(Vehiculo) %>%
  summarise(
    costo_total = mean(Costo),
    costo_total_directo = mean(CostoDirecto),
    costo_total_fijo = mean(CostoFijo)
  )

print(costo_total_por_vehiculo)

