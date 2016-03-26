#' ---
#' layout: post
#' title: Homenaje a David Bowie (y III)
#' author: José Miguel González Aguilera
#' date: 2016-03-26
#' category: análisis
#' tags: [R, análisis, bowie]
#' lang: es
#' ref: david_bowie_3
#' url-image: /images/david_bowie_3_files/figure-html/ward_clusters_5.svg
#' ---
#+ echo=F, message=F

library(magrittr)
library(tm)
library(plyr)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(reshape2)
library(FNN)

load("songs.Rda")

#' 
#' En este artículo voy a concluir el breve análisis sobre las canciones de David Bowie 
#' disponiendo un modelo sobre el que se puedan buscar las canciones más cercanas a otra.
#' 
#' En el artículo anterior hice una exploración de las canciones a través de la generación
#' de clusteres y la visualización del árbol asociado. Ahora, se tratar de disponer de un
#' modelo práctico que permita consultar dinámicamente por las canciones más próximas a una
#' determinada.
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
#' Son las mismas  
{{nrow(df_songs)}}
#' canciones de los
{{nrow(df_albums)}}
#' albumes que he utilizado en el primer análisis:
#' 
#+ echo=F, message=F, warning=F, results="asis"

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
#' 
#' En este caso, para calcular la distancia entre canciones, voy a utilizar la 
#' *frecuencia de término – frecuencia inversa de documento* (tf-idf) de cada palabra, en lugar
#' de la frecuencia absoluta de aparición de cada palabra.
#' 
#' La ventaja de este algoritmo es que da más peso a las palabras que son muy frecuentes en algunos
#' documentos, pero en un número pequeño relativo de documentos del corpus. Es decir, hace que unas
#' palabras sean más significativas que otras. Con este sistema, la nube de palabras cambia algo: 
#' 
#+ echo=F, message=F, warning=F


terms <-DocumentTermMatrix(miCorpus,control = list(weighting = weightTfIdf)) # utilizo weightTfIdf para darle peso a las palabras

mat <- as.matrix(terms)
v <- sort(rowSums(t(mat)), decreasing=TRUE)
wordcloud(names(v), v, min.freq=1, random.order = F, colors=brewer.pal(9,"Blues")[c(5,7,9)], scale=c(5, 0.5))

#' 
#' # Canciones cercanas a una determinada
#' 
#' Utilizando la matriz de términos y frecuencias basada en tf-idf se puede determinar de forma sencilla qué canciones 
#' son las más cercanas a una dada, basándonos, como en el artículo anterior, en la distancia euclídea entre canciones, 
#' utilizando los valores del vector de palabras de cada canción. 
#' 
#' Este método, al utilizar tf-idf, da mejores resultados que la clasificación en árbol del artículo anterior, que se
#' basaba en las frecuencias absolutas. Basta utilizar td-idf en el artículo anterior para conseguir resultados similares.
#' 
#+ echo=F, message=F, warning=F, results="asis"

lazarus <- terms[df_songs$song == "Lazarus", ]
cercanas_a_lazarus <- get.knnx(terms, lazarus, k = 10, algorithm = "cover_tree")
cercanas_a_lazarus <- data.frame(song = df_songs$song[cercanas_a_lazarus$nn.index], distance = c(cercanas_a_lazarus$nn.dist))

cercanas_a_lazarus %>% 
  knitr::kable(row.names=F, format="markdown")

#' 
#' Veamos qué canciones son encuentra nuestro método que son las más parecidas a _Space Oddity_: 
#' 
#+ echo=F, message=F, warning=F, results="asis"

  
major_tom <- terms[df_songs$song == "Space Oddity",]
cercanas_a_tom <- get.knnx(terms, major_tom, k = 10, algorithm = "cover_tree")
cercanas_a_tom <- data.frame(song = df_songs$song[cercanas_a_tom$nn.index], distance = c(cercanas_a_tom$nn.dist))

cercanas_a_tom %>% 
  knitr::kable(row.names=F, format="markdown")

#'
#' El algoritmo encuentra que la canción más parecida es _Ashes to Ashes_. Es decir, la canción en la que aparece
#' también el Major Tom. Esto es una mejora significativa con respecto al sistema basado en las frecuencias absolutas
#' del artículo anterior (Se puede buscar allí y se observará la diferencia).
#'
#' Probemos una más, la canción _"Heroes"_
#'   
#+ echo=F, message=F, warning=F, results="asis"

heroes <- terms[df_songs$song == '"Heroes"',]
cercanas_a_heroes <- get.knnx(terms, heroes, k = 10, algorithm = "cover_tree")
cercanas_a_heroes <- data.frame(song = df_songs$song[cercanas_a_heroes$nn.index], 
                                distance = c(cercanas_a_heroes$nn.dist))

cercanas_a_heroes %>% 
  knitr::kable(row.names=F, format="markdown")


#' 
#' # Nube de canciones
#'
#' Al tener las distancias entre las canciones, podemos visualizar estas relaciones entre ellas
#' mediante una red.
#' 
#' Si visualizásemos las distancias entre todas ellas, la red sería inmanejable, basta con incluir las relaciones
#' más significativas, Para tener una idea de lo que supone (en cualquier caso 254 canciones, crean una red muy compleja de visualizar).
#' 
#+ echo=F, message=F, warning=F


# construir la matriz de adyacencia





matriz = matrix(0, nrow = nrow(df_songs), ncol = nrow(df_songs))
colnames(matriz) = df_songs$song
rownames(matriz) = df_songs$song

for (i in 1:nrow(df_songs)) {
  t_song <- terms[i,]
  cercanas <- get.knnx(terms, t_song, k = 6, algorithm = "cover_tree")
  for (j in 2:6) {
    matriz[i, cercanas$nn.index[j]] = cercanas$nn.dist[j]
  }
}

#+ echo=F, message=F, warning=F, fig.height=8, fig.width=8

library(igraph)
g <- graph.adjacency(matriz, weighted=TRUE, mode = "undirected")
g = simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)/50
set.seed(1000)
layout1 <- layout.fruchterman.reingold(g)
V(g)$label.cex <- V(g)$degree / (10*max(V(g)$degree)) +0.2 
V(g)$label.color <- rgb(0, 0, 0.2, 0.8)
V(g)$frame.color <- NA
egam <- (log(E(g)$weight)+.65) / max(log(E(g)$weight)+.65) # evitar negativos
E(g)$color <- rgb(0.5, 0.5, 0, egam)

E(g)$width <- egam

plot(g, layout=layout1, vertex.size=V(g)$degree / max(V(g)$degree)+ 5)

#' 
#' # Referencias
#' 
#' El documento R (rmarkdown) para reproducir este análisis puede encontrarse en
#'  [este enlace](http://github.com/jmgaguilera/david_bowie)
