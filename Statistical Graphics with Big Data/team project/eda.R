library(tidyverse)

setwd("C:/Users/Chanmi Yoo/Desktop/빅데/팀프로젝트/data")
spotify <- read_csv("SpotifyFeatures.csv")
str(spotify)

spotify_ <- spotify %>% arrange(popularity) %>% tail(round(nrow(spotify)*0.01))
tail(features_pop)
ggplot(features_pop, aes(energy, tempo)) + geom_point() 

