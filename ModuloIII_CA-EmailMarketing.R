#Análisis explotatorio de campañas de E-mail Marketing

#PASO 1: Deben cargar las librerias:
library(tidyverse)
install.packages (lubridate)
library(lubridate)
library(corrplot)
library(psych)
library(ggplot2)
library(dbplyr)
#PASO 2: Generar un dataset llamado email_analysis cargando el archivo "dataset_email-mkt.csv" eliminando primera columna
email_analysis <- read.csv("dataset_email-mkt.csv", header=T)
email_analysis <- select(email_analysis,-1)
#Modificar la columna de sendout_date para que sea fecha y no character
email_analysis$sendout_date <- as.Date(email_analysis$sendout_date, "%Y-%m-%d")
class(email_analysis$sendout_date)

#PASO 3: Generar un segundo dataset email_campaign filtrando la columna
#email_scope == "Campaign"
email_campaign <- email_analysis %>% 
  filter(email_scope=="Campaign")


#Calculen los datos agregados de todas las columnas que comienzan con "total_"
#agrupando por journeys_id
suma_total <- email_campaign %>% 
  select(contains("total"), journeys_ids) %>% 
  group_by(journeys_ids) %>% 
  summarise_all(sum, na.rm=TRUE)

#Realicen un plot de la cantidad de envios de mails para cada journeys_id
id_sent <- email_campaign %>% 
  group_by(journeys_ids) %>% 
  select(total_email_sent)
Nemails_plot <- ggplot(id_sent, aes(journeys_ids)) + 
  geom_bar(fill="black", col="blue") + 
  labs(y="emails_sent") +
  scale_x_continuous(breaks=seq(1, 90, by = 3))
#PASO 4: Realizar los cálculos de open_rate y ctor para cada journeys_id
#OR: el porcentaje de emails que fueron abiertos por los
#destinatarios sobre el total de emails enviados.
#Click to Open Rate (CTOR): El porcentaje de usuarios que
#recibieron el mail, lo abrieron y realizaron clic en el link deseado.
OR_CTOR <- suma_total %>% 
  group_by(journeys_ids) %>% 
  summarise(OR=total_email_open/total_email_sent,
            CTOR=total_email_clicks/total_email_sent)

#Cual es el OR y CTOR promedio de todas las campañas realizadas?
mean(OR_CTOR$CTOR) #0.1224594
mean(OR_CTOR$OR) # 0.9524513

#Cuales son las campañas que mejor han performado?
which.max(CTOR$CTOR) #total email clicks=3 
which.max(OR$OR) #total email open = 30
summary(CTOR$CTOR)
summary(OR$OR)

#Las campañas que peor performan son aquellas donde más "flag_unsubscribe"
#con valor TRUE existen? #Existen 36 con valor TRUE
summary(email_campaign$flag_unsubscribe)
which(email_campaign$flag_unsubscribe == TRUE)

#PASO 4: Realizar análisis de los usuarios según su género, realizando un nuevo
#dataset que agregue los datos según género
#Calcular métricas de OR y CTOR para cada género e identificar si se perciben
#diferencias de comportamiento en relación a la tasa de apertura y clics
malefemale <- c("m", "f")
email_genero <- email_campaign %>%
  filter(gender %in% malefemale) %>% 
  select(gender,contains("total")) %>% 
  group_by(gender) %>% 
  summarise_all(sum, na.rm=TRUE)

OR_CTOR_genero <- email_genero %>% 
  group_by(gender) %>%
  summarise(OR=total_email_open/total_email_sent,
            CTOR=total_email_clicks/total_email_sent)
  #mujeres <- OR=0.4451702 CTOR=0.03403231  
  #hombres <- OR=0.5072000 CTOR=0.04053333

#Qué sucede con la cantidad promedio de páginas vistas por género?
#Los hombres o las mujeres exhiben un comportamiento diferencial?
page_views_genero <- email_genero %>% 
  group_by(gender) %>% 
  summarise(pageviews= total_pageviews/total_email_sent)
  #mujeres = 0.01650052
  #hombres = 0.01564444
  # No hay comportamiento diferencial el porcentaje de paginas vistas es muy
  #parecido entre ambos sexos
