cat("\014")                     # clears all the variables 
rm(list=ls(all=TRUE))           # remove all variables of the workspace

library(tidyverse)              # data manipulation 
library(dplyr)                  # can be avoided loading tidyverse
library(tidyr)                  # can be avoided loading tidyverse
library(RColorBrewer)           # Pacchetto per creare palette di colori più accattivanti
library(ggplot2)                # data visualization
library(rpart)
library(plyr)
library(partykit)
library(scales)
library(MLmetrics)

df <- read.csv("points.csv", sep=";", dec = ",")
df$labels <- paste(df$punto, "(",df$x,",",df$y,")")

dev.new()
ggplot(data = df, mapping = aes(x = x, y = y, label = punto)) + 
  geom_point()+
  geom_text(hjust = 0, nudge_x = 0.01) +
  theme_void() # white bakground with lines

ggsave("points.png",  width = 3, height = 3 )
dev.off()


dist1 <- dist(df[2:3])          # matrice dell distanze euclidee (da 4 a 99 perchè si calcola su tutti i 19 min di ciascun giorno di df1)
hc_single <- hclust(dist1, method = "single") #cluster gererchico (agglomerativo) con metodo ward
dev.new()
png(file = "single.png", bg = "white", width = 300, heigth = 300)                   # to save automatically image in WD
plot(hc_single, main = "Single link clustering")
# rect.hclust(hc_single, k=3, border="red")
dev.off()

hc_complete <- hclust(dist1, method = "complete") #cluster gererchico (agglomerativo) con metodo ward
dev.new()
png(file = "complete.png", bg = "white")                   # to save automatically image in WD
plot(hc_complete, main = "Complete link clustering")
dev.off()

hc_group <- hclust(dist1, method = "average") #cluster gererchico (agglomerativo) con metodo ward
dev.new()
png(file = "group.png", bg = "white")                   # to save automatically image in WD
plot(hc_group, main = "Group average clustering")
dev.off()

hc_ward <- hclust(dist1, method = "ward.D2") #cluster gererchico (agglomerativo) con metodo ward
dev.new()
png(file = "ward.png", bg = "white")                   # to save automatically image in WD
plot(hc_ward, main = "Ward's clustering")
dev.off()

