#' ---
#' layout: post
#' title: Homenaje a David Bowie. 2ª parte.
#' #' author: José Miguel González Aguilera
#' date: 2016-02-16
#' category: análisis
#' tags: [R, análisis, clusterización, bowie]
#' ---
#+ echo=F, message=F

library(magrittr)
library(tm)
library(plyr)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(reshape2)

load("songs.Rda")

#' 
#' Quiero aprovechar mi homenaje a David Bowie para mostrar un ejemplo
#' sencillo de utilización de técnicas de análisis textual con R.
#' 
#' De acuerdo a la terminología moderna, esto se considera hoy día una de las
#' aplicaciones para el manido *Big Data*, ya que, según la definición ampliamente aceptada,
#' aquel incluye el manejo de altos volúmenes de información y/o el manejo de 
#' información que cambia rápido (cuya utilidad es *ahora* o no sirve para nada) y/o
#' información desestructurada (como documentos de texto).
#' 
#' En este caso estamos tratando con información textual (las letras) de un conjunto de
#' documentos (las canciones).
#' 
#' Mi objetivo consiste en utilizar técnicas de *aprendizaje sin supervisión* para, a partir de
#' las letras de las canciones, tratar de agrupar las más parecidas entre sí.
#' 
#' 
#'  
#' # Los datos
#' 
#+ echo=F, message=F, warning=F
df_songs$album <- factor(df_songs$album, levels = c(levels(df_songs$album), "The Rise And Fall...", "The Man Who...")) # añade un factor
df_songs[sapply(df_songs$album, function (x) {grepl("The Rise",x)}),]$album = "The Rise And Fall..." # renombre
df_songs[sapply(df_songs$album, function (x) {grepl("The Man Who",x)}),]$album = "The Man Who..." # renombre
df_songs$album <- factor(df_songs$album)

df_albums <- df_songs %>% 
  .[c("album", "year")] %>% 
  unique() %>% .[order(.$year),]

myStopWords <- c(stopwords('english'), 'la', 'ive', 'youve', 'youre', 'chorus', 'theres', 'therell')

miCorpus <- df_songs$lyrics %>% VectorSource() %>% Corpus() %>% # corpus
            tm_map(stripWhitespace) %>% tm_map(tolower) %>% 
            tm_map(removePunctuation) %>%  # quitar lo no necesario
            tm_map(removeNumbers) %>% 
            tm_map(removeWords, myStopWords) %>% 
            tm_map(stripWhitespace) %>% 
            tm_map(function(x){unlist(strsplit(x[[1]],' '))}) 

diccionario <- unique(unlist(miCorpus, use.names=F))

miCorpus <- tm_map(miCorpus, stemDocument, language='english') %>% # stemming
            tm_map(stemCompletion, dictionary=diccionario) %>% # recupera palabras completas
            tm_map(PlainTextDocument)

dtm <- TermDocumentMatrix(miCorpus)
colnames(dtm) <- df_songs$song
# freqTerm <- findFreqTerms(dtm, lowfreq = 50)

#'
#' Antes de entrar en la clusterización, veamos antes, a modo de ejemplo, 
#' con qué otras palabras se correlaciona *god*.
#'
#'
#+ echo=F, message=F, warning=F, results="asis"

findAssocs(dtm, c("god"), corlimit=0.8)

#'
#' Otro ejemplo de correlación con la palabra *love*:
#'
#+ echo=F, message=F, warning=F, results="asis"

findAssocs(dtm, c("love"), corlimit=0.35)

#'
#' Se trata de una palabra que presenta baja correlación con otras.
#' 

#' 
#' Pasamos a presentar el resultado de la clusterización
#' 
#+ echo=F, message=F, warning=F

dtms <- removeSparseTerms(dtm, 0.95)
library(cluster)
distance <- dist(t(dtms), method="euclidian")
fit <- hclust(d=distance, method="ward.D")   
plot(fit, hang = -1, main="Árbol de grupos de canciones",
     xlab = "Canciones", ylab='Distancia entre grupos', cex=0.5)

#'
#' Dado que son 254 canciones, es muy complejo observar de una sentada quienes conforman
#' los diferentes grupos.
#' 
#' En cualquier caso, antes de entrar en detalle quiero entrar a ver el árbol de arriba a abajo.
#' Si pensamos en agrupar las canciones en 2 grupos, con este método, nos sale:
#' 
#+ echo=F, message=F, warning=F

rect.hclust(fit, k=2, border="red")

#' Si las agrupamos en 3, con este método, nos sale:
#' 
#+ echo=F, message=F, warning=F

plot(fit, hang = -1, labels=df_songs$album, main="Árbol de grupos de canciones",
     xlab = "Canciones", ylab='Distancia entre grupos', cex=0.5)
rect.hclust(fit, k=3, border="red")

#' Y si elegimos 5:
#' 
#+ echo=F, message=F, warning=F

plot(fit, hang = -1, main="Árbol de grupos de canciones",
     xlab = "Canciones", ylab='Distancia entre grupos', cex=0.5)
rect.hclust(fit, k=5, border="red")

#'
#'Con esta división, hay dos grupos minúsculos, a la derecha. Y los tres restantes están más poblados.
#'
#' Veamos primero los tres grupos más grandes
#' 
#' 

dendro <- as.dendrogram(fit)
branches <- cut(dendro, h=70)$lower
plot(branches[[1]],main="Primer grupo de canciones", cex=0.5)

plot(branches[[2]],main="Segundo grupo de canciones", cex=0.5)

plot(branches[[3]],main="Tercer grupo de canciones", cex=0.5)

plot(branches[[4]],main="Cuarto grupo de canciones")

plot(branches[[5]],main="Quinto grupo de canciones")
