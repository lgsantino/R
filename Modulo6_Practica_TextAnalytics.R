# MÓDULO VI - TEXT ANALYTICS

#En este ejercicio realizamos un análisis de la conversación en Twitter alrededor de la marca Zara.
#Realizaremos un análisis exploratorio utilizando las técnicas vistas en clase. Finalmente,
#aplicaremos un análisis de sentimiento y modelado de tópicos, que nos permitan profundizar
#en los documentos (tweets) extraídos y en la conversación alrededor de la marca.

#Para completar el ejercicio deberan cargar las siguientes librerías:
library(tidyverse) 
library(stringr)
library(rtweet)
library(readtext)
library(tidytext)
library(udpipe)
library(quanteda)
library(syuzhet)
library(topicmodels)
library(psych)

#PISTA: Las librerias fueron utilizadas en los ejercicios prácticos del módulo de Text Analytics 
#Pueden revisar esos script como referencia para esta tarea


#PASO 1
#Realizamos una búsqueda en Twitter utilizando la query de búsqueda "Zara". Fijamos los parámetros:
# n= 18000 (límite máximo permitido de descarga de registros, es posible que se crearan menos tweets en el intervalo temporal seleccionado)
# include_rts= FALSE
# lang= "es"
Tweets_Zara <- search_tweets(q="Zara", n = 18000, include_rts = FALSE, lang= "es")

summary(Tweets_Zara)
str(Tweets_Zara)
head(Tweets_Zara)

#Inspecciona el dataframe
#El dataset descargado contiene 90 columnas, pero no nos interesan todas. Selecciona las columnas:
# created_at, screen_name, text, favorite_count, retweet_count, lang, status_url, name, location, 
# description, followers_count, friends_count, statuses_count
Tweets_Zara <- Tweets_Zara %>% 
  select(created_at, screen_name, text, favorite_count, retweet_count, lang, status_url, name, location, 
         description, followers_count, friends_count, statuses_count)
#Convierte el texto en minúsculas. PISTA: utiliza la librería stringr
#Convierte la fecha de creación en Date
Tweets_Zara <- Tweets_Zara %>%
  mutate(text = str_to_lower(text), created_at = as.Date(created_at)) 

#Sustituye las letras acentuadas por letras sin acentuar. PISTA: utiliza la librería stringr
Tweets_Zara <- Tweets_Zara %>% 
  mutate(text = str_replace_all(text, '?', 'a'),
         text = str_replace_all(text, '?', 'e'),
         text = str_replace_all(text, '?', 'i'),
         text = str_replace_all(text, '?', '?'),
         text = str_replace_all(text, '?', 'u'))

#Inspecciona de nuevo el dataset con summary, str y head.
summary(Tweets_Zara)
str(Tweets_Zara)
head(Tweets_Zara)


#Verifica que el texto se transformó en minúsculas y que las letras con acento se sustituyeron por letras sin acentuar
#¿Cuántos registros (tweets) contiene el dataset?

#7299 observaciones

#Añade una nueva columna al dataset que unifique el volumen total de interacciones
#La columna se debe llamar "interacciones" y se calcula como la suma de favorite_count y retweet_count para cada registro
Tweets_Zara <- Tweets_Zara %>% 
  mutate(interacciones = rowSums(.[4:5]))



# PASO 2 
#Analizamos los datos extraídos y respondemos a preguntas sobre los datos

#Visualiza el número de tweets por día. ¿En qué día se crearon más tweets?
tweets_dia <- Tweets_Zara %>% 
  group_by(created_at) %>% 
  summarise(nº_tweets=table(created_at))
max(tweets_dia$nº_tweets)
  #El 2020-12-15 es el día con más numero de tweets, 883 tweets.

#Calcula el número total (suma) de interacciones por día. Represéntalo gráficamente
#¿En qué día hubo más interacciones?
interacciones_dia <- Tweets_Zara %>% 
  group_by(created_at) %>% 
summarise(nº_interacciones=sum(interacciones))

ggplot(interacciones_dia, aes(created_at, nº_interacciones)) + 
  geom_col(col= "black", fill = "red") +
  scale_x_date(breaks = interacciones_dia$created_at) +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("Interacciones por día")
  #El día 2020-12-15 fue el que más interacciones tuvo con 4853.

#¿Qué cuentas (screen_name) tienen mayor (max) número de followers? Pista, necesito utilizar la columna followers_count
followers <- Tweets_Zara %>%
  group_by(screen_name) %>% 
  summarise(seguidores= unique(followers_count))
  #Eltop 5 de cuentas con mas seguidores son las siguientes:
    #ElUniversal: 5079472
    #RevistaSemana: 4568360
    #biobio: 3271436
    #Reforma: 3209296
    #VogueSpain: 2707973
 
#¿Cuál fue el tuit con más retweets? Pista, necesito utilizar la columna retweet_count
which.max(Tweets_Zara$retweet_count)
max(Tweets_Zara$retweet_count)
 #El tweet 6239 de nuestro dataset es el que mas retweets acumula, 120 retweets 

#¿Cuál fue el tuit con más likes? Pista, necesito utilizar la columna favorite_count
which.max(Tweets_Zara$favorite_count)
max(Tweets_Zara$favorite_count)
  #El tweet 2232 de nuestro dataset es el que mas favoritos acumula, 1250 favoritos

#PASO 3
#Tokenizamos el texto, separándolo en palabras y contando el número de palabras.
#Filtramos menciones y visualizamos hashtags
Tokens_words_Zara <- Tweets_Zara %>% 
  unnest_tokens(input =  text, output = word, token = "words", drop = TRUE) %>% 
  count(word) %>% 
  arrange(desc(n))


#Utiliza la función unnest_tokens() de la librería tidytext para tokenizar el texto
#Cuenta el número de palabras y ordenálas en orden descendente según su frecuencia
#PISTA: utiliza el parámetro token= "tweets". Consulta la ayuda de la función unnest_tokens()
?unnest_tokens
Tokens_tweets_Zara <- Tweets_Zara %>% 
  unnest_tokens(input =  text, output = word, token = "tweets", drop = TRUE) %>% 
  count(word) %>% 
  arrange(desc(n))
Menciones_Zara <- Tokens_tweets_Zara %>%
  filter(str_detect(word, pattern = "@[:alnum:]"))

#Excluye menciones del dataframe tokenizado Tweets_Zara_Token.
#PISTA: utiliza filter() junto con str_detect() con pattern = "@[:alnum:]", consulta el script 2_Libreria_RTWEET
Tokens_tweets_Zara <- Tokens_tweets_Zara %>% 
  filter(str_detect(Tokens_tweets_Zara$word, pattern = "@[:alnum:]*", negate = TRUE))


#Crea un dataframe que contenga los hashtags. PISTA: consulta el script 2_Libreria_RTWEET
Hashtags_Zara <- Tokens_tweets_Zara %>%
  filter(str_detect(word, pattern = "#[:alnum:]"))

#Representamos los hashtags como un wordcloud utilizando la librería wordcloud, que no fue introducida en las sesiones prácticas
#Puedes hacer pruebas y variar los parámetros max.words, min.freq, scale, etc para observar como varía el resultado
#install.packages("wordcloud")
library(wordcloud)
wordcloud(
  words = Hashtags_Zara$word, 
  freq = Hashtags_Zara$n, 
  max.words = 80,
  min.freq = 4,
  scale =c(2.8,0.75),
  random.order = T, 
  rot.per = 0.3, random.color = T,
  color = brewer.pal(4, "BrBG"))


#PASO 4
#Realizamos un análisis de sentimiento utilizando la librería SYUZHET
#A diferencia del script 6_Libreria_SYUZHET, donde aplicamos un análisis de sentimiento por palabra (token),
#en este caso apliqueremos la función get_nrc_sentiment a cada tweet (documento)

#Como el dataset es relativamente grande, en esta sección trabajaremos con una muestra.
#Seleccionamos una muestra de 500 tweets de forma aleatoria utilizando la función sample.
Dataset_Zara_subset <- Tweets_Zara[sample(nrow(Tweets_Zara), size=500), ]

#La función get_nrc_sentiment de la librería Syuzhet permite visualizar las emociones y sentimiento
#Analiza 8 "emociones": anger, anticipation, disgust, fear, joy, sadness, surprise y trust
#Así como la polaridad positivo o negativo.

#Utilizamos la función get_nrc_sentiment() con el parámetro language= "spanish"
Analisis_NRC <- get_nrc_sentiment(char_v = Dataset_Zara_subset$text, language = "spanish")

#Inspecciona el resultado utilizando View()
view(Analisis_NRC)

#Unificamos el resultado y el dataframe de partida Dataset_Zara_subset, utilizando la función cbind()
Analisis_NRC_df <- cbind(Dataset_Zara_subset$text, Analisis_NRC)

#Inspecciona de nuevo el resultado utilizando summary
# Observa los valores mínimo, máximo y medio para cada una de las 8 emociones y para las columnas negative/positive
summary(Analisis_NRC_df)

# 1) Calcula la suma total de la columna positive
sum(Analisis_NRC_df$positive)
# 2) Calcula la suma total de la coluna negative.
sum(Analisis_NRC_df$negative)
# ¿La polaridad de la conversación es positiva o negativa?. PISTA: resta el total negativo al total positivo
sum(Analisis_NRC_df$negative) - sum(Analisis_NRC_df$positive)#-27
 #Es positiva

#Finalmente podemos analizar el porcentaje de cada emoción en la conversación
#Solución: utilizamos la función prop.table y colSums para obtener el porcentaje de cada emoción
# La función prop.table divide el valor de cada celda entre la suma total de filas y columnas (% de la celda)
# La función colSums() suma el valor de todas las celdas de cada columna (% de la columna)
Analisis_NRC_emotions <- colSums(prop.table(Analisis_NRC_df[c("anger", "anticipation", "disgust", "fear",
                                                              "joy", "sadness", "surprise", "trust")])) 
sort(Analisis_NRC_emotions*100, decreasing= TRUE)

#Inspeccionamos ejemplos
angry_items <- which(Analisis_NRC_df$anger > 0)
Analisis_NRC_df[angry_items, "text"]

joy_items <- which(Analisis_NRC_df$joy > 0)
Analisis_NRC_df[joy_items, "text"]

#Nota: este ejercicio es un ejemplo de cómo trabajar con la librería Syuzhet y realizar un análisis de sentimiento
# En un caso real, se debe analizar y limpiar en profundidad el conjunto de documentos (tuits en este caso),
# por ejemplo eliminando menciones, urls y documentos (tuits) no relevantes del análisis de sentimiento.


#PASO 5
#Analizamos el dataset utilizando la libería udpipe.

#Descargamos y cargamos el modelo para español. 
ud_model <- udpipe_download_model(language = "spanish")
#ud_model <- udpipe_load_model(ud_model$file) #Esta línea no se ejecuta correctamente si existe más de un modelo en el directorio de nuestro ordenador
ud_model <- udpipe_load_model(file= "spanish-gsd-ud-2.5-191206.udpipe") #Al especificar el nombre del modelo a cargar, aseguramos que sí cargue el modelo correctamente


#Lo aplicamos sobre la columna del texto de tuits, generando 14 variables
Dataset_Zara_ud <- udpipe_annotate(ud_model,
                                   x = Tweets_Zara$text,
                                   parallel.cores = 2)

#Convertimos en data frame. Inspecciona el resultado, revisa las variables generadas por la función udpipe_annotate()
Dataset_Zara_ud <- as.data.frame(Dataset_Zara_ud)


#Observa que los signos de puntuación no han sido eliminados
#Utilizando la columna "token", elimina los signos de puntuación y las menciones
#PISTA: para eliminar signos de puntuación utiliza el patrón "[:punct:]". Revisa la cheatsheet de stringr vista en clase.
Dataset_Zara_ud <- Dataset_Zara_ud %>%
  select(token)
  filter(str_detect(token, pattern = "@[:alnum:]", negate = TRUE)) %>% 
  mutate(token = str_replace_all(Review, '[[:digit:]]', ' ')) %>%
  mutate(token = str_replace_all(Review, '[[:punct:]]', ' ')) 

#Analicemos los adjetivos
Adjetivos_Zara <- Dataset_Zara_ud %>%
  filter(upos == "ADJ") %>%
  count(token) %>%
  arrange(desc(n))

wordcloud(
  words = Adjetivos_Zara$token, 
  freq = Adjetivos_Zara$n, 
  max.words = 80,
  min.freq = 5,
  scale =c(4.8,0.4),
  random.order = T, 
  rot.per = 0.3, random.color = T,
  color = brewer.pal(4, "BrBG"))

#Analiza los verbos y representa un wordcloud como hemos hecho en el caso de los adjetivos
#PISTA: utiliza la condición de filtrado upos == "VERB"
Verbos_Zara <- Dataset_Zara_ud %>%
  filter(upos == "VERB") %>%
  count(token) %>%
  arrange(desc(n))

#Nota: observa que "Zara" ha sido incorrectamente clasificado como Adjetivo y como Verbo.
#De la misma forma, otros tokens no fueron clasificados correctamente.
#En un caso real, sería necesario corregir estos defectos en la anotación del dataframe.

#Leemos el resultados de los pasos anteriores
Dataset_Zara_ud <- read.csv("datasets/Dataset_Zara_ud.csv")


#PASO 6
#Realizamos un modelado de tópicos utilizando la librería topicmodels
#El objetivo es identificar temas en la conversación en Twitter sobre la marca Zara

#Para ello realizamos los siguientes pasos:
# - Seleccionamos nombres y adjetivos
# - Excluímos palabras muy frecuentes en los documentos pero sin significado relevante,
#   como el término de búsqueda de tuits "Zara" o palabras como "gracias", "por favor", etc.
# - Trabajamos con el id de documento (doc_id) y el lema (token lematizado)

#Nota: la libería topicmodels está construida utilizando objetos del paquete tm. Para poder ejecutar funciones
#de este paquete, debemos transformar en Document Term Matrix (dtm) utilizando la función cast_dtm()
Modelo_Zara <- Dataset_Zara_ud %>% 
  filter(upos %in% c("NOUN", "ADJ")) %>% 
  filter(!token %in% c("zara", "gracias", "por", "favor", "vez")) %>%
  select(id = doc_id, word = lemma) %>%
  mutate(id = str_replace_all(id, "doc", "")) %>% 
  count(word, id) %>% 
  cast_dtm(id, word, n)

#Generamos varios modelos, variando el número de temas definido en cada modelo (parámetro k)
#Utilizamos la función LDA() del paquete topicmodels
set.seed(1234)
Modelo_Zara_LDA <- LDA(Modelo_Zara, k = 3, control = list(seed = 1234))
Modelo_Zara_LDA2 <- LDA(Modelo_Zara, k = 5, control = list(seed = 1234))
Modelo_Zara_LDA3 <- LDA(Modelo_Zara, k = 8, control = list(seed = 1234))

#Transformamos en formato tidy (tibble data.frame) utilizando la función tidy()
Zara_topics <- tidy(Modelo_Zara_LDA, matrix = "beta")
Zara_topics2 <- tidy(Modelo_Zara_LDA2, matrix = "beta")
Zara_topics3 <- tidy(Modelo_Zara_LDA3, matrix = "beta")

#Inspecciona los dataframes. Puedes realizar una primera inspección ordenando de forma descendente utilizando la columa beta


#Seleccionamos los top terms de cada modelo y los visualizamos
## Modelo k=3
Zara_top_terms <- Zara_topics %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

Zara_top_terms_facet <- Zara_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

## Modelo k=5
Zara_top_terms2 <- Zara_topics2 %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

Zara_top_terms_facet2 <- Zara_top_terms2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

## Modelo k=8
Zara_top_terms3 <- Zara_topics3 %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

Zara_top_terms_facet3 <- Zara_top_terms3 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  theme(axis.text=element_text(size=7)) +
  scale_x_reordered() #reorder_within y scale_x_reordered permiten ordenar en cada facet (tema)


#Seleccionamos el modelo k=5
#Intenta describir cada uno de los 5 temas identificados, revisando las palabras con mayor probabilidad
#(beta) de pertenecer a cada tema
#PISTA: ejecuta el objeto Zara_top_terms_facet2 para realizar un análisis visual
  #En el grafico se ven 5 segmentos de palabras en jerquia de importancia.
  #En 1er segmento podemos destacar palabras como navidad, año, vestido y bershka 
  #lo que deja entrever un competidor de zara en posiblemente unas fechas señaladas.
  #Unos segmentos 2 y tres en el que se puede destacar palabras como tienda, pedido,
  #numero, nombre o día que nos señala que posiblemente se este hablando de el servicio de 
  #envios de Zara.
  #El segmento 4 no arroja palabras muy significatvas y en el 5 se puede destacar la palabra
  #casa en la que se puede hacer la interpretación de que se puede estar hablando de la filial
  #de Zara, ZaraHome.


#Finalmente, utilizando un wordcloud visualizamos las palabras más relevantes por tópico.
#Por ejemplo, para el tópico número 2
Zara_wordcloud <- Zara_topics2 %>%
  filter(topic == "2") %>%
  arrange(topic, -beta) %>% 
  mutate(beta = beta * 1000)

wordcloud(
  words = Zara_wordcloud$term, 
  freq = Zara_wordcloud$beta, 
  max.words = 40,
  min.freq = 5,
  scale =c(4.5,0.2),
  random.order = F, 
  rot.per = 0.1, random.color = T,
  color = brewer.pal(4, "BrBG"))


