# generate dataset

library(dplyr)
library(magrittr)

ficheros = list.files(path="songs", pattern="*.csv")
df_songs <- ficheros %>% 
            lapply(function(i) {paste("songs/",i, sep='')}) %>% 
            lapply(function(i) {read.csv(i, header=F, stringsAsFactors=F)}) %>%
            rbind_all()
names(df_songs) <- c('song', 'album', 'year', 'url', 'lyrics')
df_songs$album <- as.factor(df_songs$album)
df_songs$year <- as.factor(df_songs$year)
save(df_songs, file="songs.Rda")