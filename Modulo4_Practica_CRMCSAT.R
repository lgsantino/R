#M脫DULO IV - SATISFACCI脫N DEL CONSUMIDOR
#En este ejercicio estaremos trabajando con data t铆pica que uno puede extraer del modulo de Support Service
#de un CRM sobre los problemas que se reportan con el producto o servicio
#Y contiene una encuesta de satisfaccion tanto para el producto como para el servicio brindado por el equipo de custome support

#Para completar el ejercicio deberan cargar las siguientes librerias: tidyverse, corrplot, psych
library(tidyverse)
library(corrplot)
library(psych)
library(ggplot2)
#PISTA: Las librerias de corrplot y psych fueron utilizadas en el ejercicio de Percepcion de Marca. 
#Pueden revisar ese script como referencia para esta tarea


#PASO 1
#Cargamos el dataset "data_CRM.csv" eliminando columna 1 que es innecesaria
#Inspecciones en dataset con summary, describe y head para comprender la informaci贸n que contiene, entender missing values en las columnas
#Realicen un plot para entender la distribuci贸n de las quejas a lo largo del per铆odo, 
#selecciondo el tipo de gr谩fico m谩s 贸ptimo para representar la data
setwd("C:/Users/luisg/Desktop/M醩ter Big Data/Customer Analytics/m閠ricas de satisfaci髇/CA_ModuloIV_PracticaCRM")
CRM <- read.csv("data_crm.csv", header = TRUE)
CRM <- select(CRM, -1)
CRM <- CRM %>% 
  filter (!is.na(ProdCSAT), !is.na(ServCSAT))
summary(CRM);describe(CRM);head(CRM)
ggplot(CRM, aes(cust_id)) + 
  geom_bar()

#PASO 2
#El dataset presenta todos los casos de los usuarios que han contactado con Customer Support en el per铆odo enero-abril 2020
#Pero nos interesa hacer el an谩lisis sobre complaint_reason, por lo cual es necesario crear un nuevo dataset, 
#agrupar los datos por complaint_reason y realizar las siguientes operaciones para las columnas relevantes: 
  #generar una columna que cuente la cantidad de llamadas para cada tipo de complaint_reason llamada "num_casos"
  #generar una columna que cuente la cantidad de llamadas pendientes para cada tipo de complaint_reason contando la cantidad de "y" llamada pend_calls
  #calcular el promedio de time_to_resolution_min para cada tipo de complaint_reason en una columna nueva llamada avg_time_to_resolution
#generar una columna que cuente la cantidad de need_replace para cada tipo de complaint_reason contando la cantidad de "TRUE" llamada n_replacements
#generar una nueva columna que calcule el Prod_CSAT para cada tipo de complaint_reason en una columna nueva llamada Prod_CSAT
#generar una nueva columna que calcule el Serv_CSAT para cada tipo de complaint_reason en una columna nueva llamada Serv_CSAT
#De esta forma el dataset nuevo debe contener las siguientes columnas: complaint_reason, num_casos, pend_calls, 
#avg_time_to_resolution, n_replacements, Prod_CSAT, Serv_CSAT
CRM_complaints <- CRM %>%
  group_by(complaint_reason) %>% 
  summarise(num_casos= table(complaint_reason))

df_pendingcall <- CRM %>% 
  select(complaint_reason, pending_call) %>% 
  filter(pending_call == 'y') %>% 
  group_by(complaint_reason)
df_pendingcall <- as.data.frame((table(df_pendingcall)))

CRM_complaints <- cbind(CRM_complaints, pend_calls = df_pendingcall$Freq)

time_resolution <- CRM %>% 
  select(time_to_resolution_min, complaint_reason) %>% 
  group_by(complaint_reason) %>% 
  summarise_all(mean)
CRM_complaints <- cbind(CRM_complaints,avg_time_to_resolution=time_resolution$time_to_resolution_min)

replace <- CRM %>% 
  select(complaint_reason, need_replace) %>% 
  filter(need_replace=='TRUE') %>% 
  group_by(complaint_reason)
df_need_replace <- as.data.frame(table(replace))
CRM_complaints <- merge(CRM_complaints, df_need_replace, by='complaint_reason', all=TRUE)

CRM_complaints <- CRM_complaints[,-5]

names(CRM_complaints)[5]='n_replacements'

CRM_complaints$n_replacements[is.na(CRM_complaints$n_replacements)] <- 0

meanPSAT <- CRM %>% 
  select(ProdCSAT, complaint_reason) %>%
  group_by(complaint_reason) %>% 
  summarise_all(mean)
CRM_complaints <- cbind(CRM_complaints, Prod_CSAT=meanPSAT$ProdCSAT)

meanSSAT <- CRM %>% 
  select(ServCSAT, complaint_reason) %>%
  group_by(complaint_reason) %>% 
  summarise_all(mean)
CRM_complaints <- cbind(CRM_complaints, Serv_CSAT=meanSSAT$ServCSAT)



#PASO 3
#Seleccionar un plot idoneo para poder realizar una comparativa de C-SATs para cada problema t茅cnico
#Justificar la selecci贸n de dicho plot brevemente
ggplot(CRM_complaints,aes(Prod_CSAT, Serv_CSAT, col=complaint_reason)) +
  geom_point(size=3, alpha=8/10)+
  labs(x='Prod_CSAT', y='Serv_CSAT')+
  theme(legend.text = element_text(size = 8))
  #Con este plot, se puede ver que patron siguen los problemas tecnicos tanto para la variable
  #Prod_CSAT como para la variable Serv_CSAT. Se puede observar que el Serv_CSAT tiende a tener valores mas altos 
  #y que para la mayoria de los problemas tecnicos estas dos variables tienen un valor similar


#PASO 4
#Realizar una correlaci贸n entre las variables num茅ricas y analizar si existen correlaciones fuertes entre
#alguna de las variables presentadas. 
#las funciones de correlaci贸n poseen un argumento llamado use que permite excluir los NA para que el computo sea
#posible. Para ello incluyan como argumento use = "complete.obs" ya que por default es use = "everything" 
#驴La columna de Serv_CSAT muestra correlaciones con alguna otra columna?
cor(CRM$age, CRM$time_to_resolution_min, use='complete.obs') #-0.003984088

cor(CRM$age, CRM$ServCSAT, use = 'complete.obs')#0.004264296
cor(CRM$time_to_resolution_min, CRM$ServCSAT, use = 'complete.obs')#0.002296477
cor(CRM$ProdCSAT, CRM$ServCSAT, use = 'complete.obs')#-0.004230716
  #No muestra correlacion con ninguna columna

#Inspeccionen la funcion cor.test para entender su funcionamiento y apliquenla sobre aquellas correlaciones
#que ustedes opinaron anteriormente que tienen correlaci贸n con la columna de Serv_CSAT para verificar si su hipotesis es correcta
#IMPORTANTE: pueden explorar los diferentes m茅todos, pero el que utilizamos de forma gen茅rica es pearson
##a su vez es importante que comprendan y utilicen el argumento exact con l贸gica FALSE
cor.test(CRM_complaints$Serv_CSAT, CRM_complaints$avg_time_to_resolution, method = "kendall", exact=FALSE) 
cor.test(CRM_complaints$Serv_CSAT, CRM_complaints$Prod_CSAT, method = "kendall", exact=FALSE)

cor.test(CRM_complaints$Serv_CSAT, CRM_complaints$avg_time_to_resolution, method = "spearman", exact=FALSE)  
cor.test(CRM_complaints$Serv_CSAT, CRM_complaints$Prod_CSAT, method = "spearman", exact=FALSE) 



#Por 煤ltimo utilicen la funci贸n corrplot.mixed() para realizar el plot de todas las correlaciones juntas
#Intenten utilizar algunas de las opciones que presenta para embellecer el gr谩fico (colores, formas, tama帽os, etc)
#La forma de aplicaci贸n ser铆a corrplot.mixed(corr = (correlacion que quieren hacer con sus argumentos incluido use = "complete.obs")) 
#y el resto de argumentos que quieran incluir
matrix_cor <-  round(cor(CRM_complaints[2:7]),
                     digits = 2)


corrplot.mixed(matrix_cor, lower = "number", upper = "number",
               tl.pos = "lt", tl.col = "black", tl.offset=1, tl.srt = 0)


#PASO 5
#Repetir el paso 4 pero enfocando el analisis en la columna Prod_CSAT en vez de Serv_CSAT: realicen hipotesis sobre correlaciones,
#apliquen cor.test para validarlas y corrplot.mixed() para representarlo.
cor.test(CRM_complaints$Prod_CSAT, CRM_complaints$avg_time_to_resolution, method = "kendall", exact=FALSE) 
cor.test(CRM_complaints$Prod_CSAT, CRM_complaints$Serv_CSAT, method = "kendall", exact=FALSE)

cor.test(CRM_complaints$Prod_CSAT, CRM_complaints$avg_time_to_resolution, method = "spearman", exact=FALSE)  
cor.test(CRM_complaints$Prod_CSAT, CRM_complaints$Serv_CSAT, method = "spearman", exact=FALSE) 

corrplot.mixed(matrix_cor, lower = "square", upper = "circle",
               tl.pos = "lt", tl.col = "black", tl.offset=1, tl.srt = 0)




