#carga de libreria 
install.packages("tidyverse")
library(tidyverse)
#Carga del cojunto de datos 
datos <- diamonds
#Nombres de las columnas del conjunto de datos
names(datos)
#Estructura de datos 
str(datos)
#Tabla de distribución de frecuencia
#Según tipo de corte
tabla_1<- datos%>%
  group_by(cut) %>% 
  summarize(contar=n()) %>% 
  mutate(porcentaje=contar/sum(contar)*100)
#graficos
pie(tabla_1$contar, tabla_1$cut, main="Tipos de Corte")
plot(tabla_1$cut,tabla_1$contar)
#Según precio 
install.packages("fdth")
library(fdth)
tabla1 <- fdt(datos$carat,k=4)
plot(tabla1, main="Frecuencia de Pesos (Quilates)", xlab="Intervalos Quilates", ylab= "Frecuencia")
#Tabla de Contigencia 
#Color y corte
tabla <- datos %>% 
  group_by(color,cut) %>% 
  summarize(n=n()) %>% 
  spread(cut,n) 
class(tabla)
tabla
# Medidas de resumen
summary(datos$price)
install.packages("ggplot2")
library(ggplot2)
ggplot(datos)+aes(y=price)+geom_boxplot(outlier.colour="red")+coord_flip()+labs(y="Precio")+ggtitle("Distribución de Presios")
q0 <- min(datos$preci)
q1 <- quantile(datos$price,25/100)
q2 <- quantile(datos$price, 50/100)
q3 <- quantile(datos$price, 75/100)
q4 <- max(datos$price)
# Tabla
install.packages("RcmdrMisc")
library(RcmdrMisc)
numSummary(datos$price,groups=datos$cut, statistics= c("quantile", "mean"))