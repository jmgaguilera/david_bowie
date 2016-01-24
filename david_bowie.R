#' ---
#' layout: post
#' title: Homenaje a David Bowie
#' author: José Miguel González Aguilera
#' date: 2016-01-24
#' category: análisis
#' tags: [R, análisis, bowie]
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
#' Ahora que todo el mundo habla parabienes de David Bowie tengo que decir que en mis tiempos de
#' juventud no me gustaba especialmente. Lo redescubrí hace pocos años en su disco _The Next Day_
#' que acabé adquiriendo en vinilo, lo que, en una época en la que ya no compro Compact Disc,
#' es un honor reservado a aquellos discos que superan la escucha en streaming.
#' 
#' Después de escuchar _Lazarus_ unos días antes de su muerte, la noticia no me sorprendió y las
#' estrofas de la canción adquirieron todo su sentido.
#' 
#' | Look up here, I’m in heaven
#' | I’ve got scars that can’t be seen
#' | I’ve got drama, can’t be stolen
#' | Everybody knows me now
#' 
#' | Look up here, man, I’m in danger
#' | I’ve got nothing left to lose
#' | I’m so high it makes my brain whirl
#' | Dropped my cell phone down below
#' |
#' 
#' 
#' Este fin de semana, mientras escuchaba el disco _Blackstar_ he querido hacer mi pequeño 
#' homenaje con este análisis de sus letras.
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

#' 
#' He desarrollado un script en Python que accede a AZLyrics, y con él he recopilado 
#' las letras de las 
{{nrow(df_songs)}}
#' canciones de los
{{nrow(df_albums)}}
#' albumes:
#' 
#+ echo=F, message=F, warning=F, results="asis"

df_albums %>% 
  knitr::kable(row.names=F, format="markdown")

#' # De qué van sus canciones
#' 
#' Esta es la nube de palabras de toda su carrera musical:
#' 
#+ echo=F, message=F, warning=F


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
freqTerm <- findFreqTerms(dtm, lowfreq = 50)

mat <- as.matrix(dtm)
v <- sort(rowSums(mat), decreasing=TRUE)
wordcloud(names(v), v, min.freq=50, random.order = F, colors=brewer.pal(9,"Blues")[c(5,7,9)], scale=c(5, 0.5))

#'
#' A modo de ejemplo, esta sería la de su disco *Space Oddity*:
#' 
#+ echo=F, message=F, warning=F

dtm <- TermDocumentMatrix(miCorpus[df_songs$album=="Space Oddity"])
freqTerm <- findFreqTerms(dtm, lowfreq = 4)

mat <- as.matrix(dtm)
v <- sort(rowSums(mat), decreasing=TRUE)
wordcloud(names(v), v, min.freq=4, random.order = F, colors=brewer.pal(9,"Greens")[c(5,7,9)], scale=c(5, 0.5))

#' 
#' ¿Veis al Mayor Tom llamando al control de tierra?
#'
#'
#' Y esta la de su último disco *Blackstar*:
#' 
#+ echo=F, message=F, warning=F

dtm <- TermDocumentMatrix(miCorpus[df_songs$album=="Blackstar"])
freqTerm <- findFreqTerms(dtm, lowfreq = 3)

mat <- as.matrix(dtm)
v <- sort(rowSums(mat), decreasing=TRUE)
wordcloud(names(v), v, min.freq=3, random.order = F, colors=brewer.pal(9,"Reds")[c(5,7,9)], scale=c(5, 0.5))

#' # Las palabras a través de los años
#' 
#' Por hacer algo diferente a lo que se ve en todos los tutoriales sobre análisis de texto vay a 
#' mostrar algunas palabras importantes y su frecuencia a lo largo de los años: god, love, time, star, live & die
#' 
#' ## God & Love
#' 
#+ echo=F, message=F, warning=F, fig.height=8, fig.width=8

cal_freq_word <- function(x, miCorpus, df_songs, word){
  dtm <- miCorpus[df_songs$album==x] %>% TermDocumentMatrix()
  freq <- rowSums(as.matrix(dtm))
  names(freq) <- rownames(dtm)
  freq <- freq[names(freq) == word]
  if (length(freq) == 0) 
    freq = c(0)
  names(freq) = c(word)
  freq
}

gg_datos <- df_albums
gg_datos <- cbind(gg_datos, gg_datos$album %>% ldply(cal_freq_word, miCorpus, df_songs, "god"))
gg_datos <- cbind(gg_datos, gg_datos$album %>% ldply(cal_freq_word, miCorpus, df_songs, "love"))

gg_datos <- melt(gg_datos, value.name="freq", variable.name="word")
gg_datos$album<-reorder(gg_datos$album, as.numeric(gg_datos$year))

ggplot(data=gg_datos,
       aes(x=album, y=freq, group=word, colour=word)) +
  geom_line(size=2) +
  theme(text = element_text(size=20), axis.text.x=element_text(angle = -90, hjust = 0))

#' 
#' ## Time & Star
#' 
#+ echo=F, message=F, warning=F, fig.height=8, fig.width=8

gg_datos <- df_albums
gg_datos <- cbind(gg_datos, gg_datos$album %>% ldply(cal_freq_word, miCorpus, df_songs, "time"))
gg_datos <- cbind(gg_datos, gg_datos$album %>% ldply(cal_freq_word, miCorpus, df_songs, "star"))

gg_datos <- melt(gg_datos, value.name="freq", variable.name="word")
gg_datos$album<-reorder(gg_datos$album, as.numeric(gg_datos$year))

ggplot(data=gg_datos,
       aes(x=album, y=freq, group=word, colour=word)) +
  geom_line(size=2) +
  theme(text = element_text(size=20), axis.text.x=element_text(angle = -90, hjust = 0))

#' 
#' ## Die & Live
#' 
#' En los dos últimos discos *die* aparece más que en los anteriores...
#' 
#+ echo=F, message=F, warning=F, fig.height=8, fig.width=8

gg_datos <- df_albums
gg_datos <- cbind(gg_datos, gg_datos$album %>% ldply(cal_freq_word, miCorpus, df_songs, "die"))
gg_datos <- cbind(gg_datos, gg_datos$album %>% ldply(cal_freq_word, miCorpus, df_songs, "live"))
gg_datos <- cbind(gg_datos, gg_datos$album %>% ldply(cal_freq_word, miCorpus, df_songs, "life"))
gg_datos$live <- gg_datos$live + gg_datos$life
gg_datos$life <- NULL

gg_datos <- melt(gg_datos, value.name="freq", variable.name="word")
gg_datos$album<-reorder(gg_datos$album, as.numeric(gg_datos$year))

ggplot(data=gg_datos,
       aes(x=album, y=freq, group=word, colour=word)) +
  geom_line(size=2) +
  theme(text = element_text(size=20), axis.text.x=element_text(angle = -90, hjust = 0))

#' # Referencias
#' 
#' El código de este programa puede encontrarse en: 
