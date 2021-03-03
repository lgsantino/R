#Practica Metricas de Satisfaccion del Consumidor - NPS
#NPS - Net Promoter Score - Score promedio de recomendacion
#En R existe una libreria llamada NPS para calcularlo automaticamente
#Deberán trabajar el calculo NPS sobre un dataset y analizar correlaciones con cluster de RFM
install.packages("NPS")

#PASO 1: Deben cargar las librerias tidyverse, lubridate, caret, corrplot y NPS
library(tidyverse)
library(lubridate)
library(caret)
library(ggplot2)
library(corrplot)
library(NPS)
library(dplyr)
#PASO 2: Generen un dataset llamado ventas2020 cargando el archivo "NPS_T1.csv" eliminando primera columna
#Eliminen aquellos registros que figuren con un NPS NA utilizando filter
#Modifiquen la columna nps a columna numerica utilizando mutate y as.numeric
#Ayuda: al utilizar select si escriben select(-1) entonces se seleccionan todas las columnas excepto la primera
ventas2020 <- read.csv("NPS_T1.csv", header=TRUE)
ventas2020 <- select(ventas2020, -1) # o tambien ventas2020 <- ventas2020[,-1]
ventas2020 <- ventas2020 %>% 
  filter(!is.na(nps))
ventas2020 <- ventas2020 %>% 
  mutate(nps = as.numeric(nps))
str(ventas2020)
#Calculen el NPS agrupado por cada tienda, utilizando la función nps del paquete NPS dentro de la funcion summarise
#¿cual es la tienda con mejor performance?
#EL NPS VA DESDE 1 A -1
#La tienda com mejor NPS es la tienda_4 con un 0.1922868
nps_stores <- ventas2020 %>%
  group_by(store) %>%
  summarise(nps(nps))
str(nps_stores)

#Realizaremos un analisis entre los meses del año y el NPS para cada tienda
#para ello deben crear una columna mes en el dataframe de ventas2020 
#y agrupar tanto por mes como por store y calcular el nps en un nuevo data frame
ventas2020 <- ventas2020 %>% 
  mutate(fecha_compra = as.Date(fecha_compra))
ventas2020 <- ventas2020 %>% 
  mutate(month=month(fecha_compra, label = TRUE))
NPS_store_month <- ventas2020 %>% 
  group_by(month, store) %>% 
  summarise(nps=nps(nps))


#visualizamos la comparación de NPS de cada tienda para cada mes
#utilicen gráfico de scatter (geom_point) y den color a los puntos con
#columna store
ggplot(NPS_store_month, aes(month, nps)) + 
  geom_point(aes(col=store), size=3.5) + 
  theme_bw()


#Desarrollar el cálculo de RFM para cada comprador en otro dataframe 
#sin olvidar de modificar la columna de  fecha para que R la reconozca como tal utilizando as.Date
#Generen 5 clusters a traves de kmean para identificar segmentos de consumidores
#pueden utilizar de referencia el script visto en el modulo II
ventas2020$fecha_compra <- as.Date(ventas2020$fecha_compra, "%Y-%m-%d")
Calculo_RFM <- ventas2020 %>% 
  group_by(id_member) %>%
  summarise(Recency=as.numeric(as.Date(Sys.Date())-max(fecha_compra)),
            Frequency=length(id_member), 
            Monetary_Value= sum(gasto))

set.seed(1234)
segmentacion_RFM <- kmeans(scale(Calculo_RFM[,2:4]), 5, nstart = 1)

Calculo_RFM$categorias_RFM <- as.factor(segmentacion_RFM$cluster)


#crear un nuevo df con media de r, f y m y el nps de nps agrupando por categoria RFM
#Calcular nps agrupando por segmento de consumidores en un nuevo data frame
Ventas_Calculo <- ventas2020 %>% 
  inner_join(Calculo_RFM, by = "id_member") 

nps_segment <- Ventas_Calculo %>% 
  select(categorias_RFM, nps, Recency, Frequency, Monetary_Value) %>% 
  group_by(categorias_RFM) %>% 
  summarise(NPS= nps(nps),
            media_recency= mean(Recency), 
            media_frequency= mean(Frequency), 
            media_monvalue= mean(Monetary_Value))


#Ahora realicen una correlacion entre NPS y  los segmentos de consumidores de RFM
#Existen mayor correlacion con aquellos consumidores que gastan mas dinero o menos dinero?
nps_segment <- nps_segment %>% 
  mutate(categorias_RFM = as.numeric(categorias_RFM))
cor.test(nps_segment$NPS, nps_segment$categorias_RFM) #0.6934793 
cor.test(nps_segment$NPS, nps_segment$media_monvalue) #-0.5922339 
  #Cuanto m�s alto el nps menos dinero se gastan los consumidores y viciversa
  #esto se puede saber ya que la correlaci�n entre estas dos variables es negativa
  
  #En el caso del nps para los segmentos, p-valor=0.1941 por tanto, se acepta la 
  #hipotesis nula y no hay correlaci�n
  
  #En el caso del nps para monetary value, p-valor=0.2927 por tanto, se acepta la 
  #hipotesis nula por tanto no hay correlaci�n

#Que sucede si realizamos un promedio de NPS por cada segmentos para cada tienda?
#los segmentos puntúan muy diferente a cada tienda? Observamos algun patron?
store_nps_segment <- Ventas_Calculo %>% 
  select(store, nps, categorias_RFM, Recency, Frequency, Monetary_Value) %>% 
  group_by(store, categorias_RFM ) %>% 
  summarise(NPS= nps(nps),
            media_recency= mean(Recency), 
            media_frequency= mean(Frequency), 
            media_monvalue= mean(Monetary_Value))

ggplot(store_nps_segment, aes(categorias_RFM, NPS, fill=store)) +
  geom_col(col='black')+
  theme_bw()
  #Como se muestra en el gr�fico Observamos que los segmentos puntuan muy similar a cada tienda

#Que sucede si correlacionamos frecuencia de compra de los 172 ids con el NPS? 
#Los consumidores que tienen mayor frecuencia de compra puntúan mas y mejor?
frec_id_nps <- Ventas_Calculo %>%
  group_by(id_member) %>% 
  summarise(nps= nps(nps),
            media_frequency= mean(Frequency))
cor.test(frec_id_nps$nps, frec_id_nps$media_frequency) #-0.08367377 
  #No hay una relaci�n significativa entre los consumidores con mayor frecuencia
  #y su puntuaci�n con una cor= -0.08367377

#En líneas generales luego del análisis exploratorio, ¿podriamos identificar tiendas que sobresalen
#por una buena o una mala performance en terminos de NPS?
  #En general tienen todas unos nps muy similares. Solo dependen de variables como el tiempo,
  #como en nuestro grafico de nps/month y de los segmentos de clientes, que muestran que la relacion 
  #entre segmentos y nps si evidencian que unas tiendas tienen un mejor nps que otras



#Pueden utilizar los dataset NPS_T2.csv y NPS_T3.csv para seguir practicando
#Y realizando cruces de informacion


