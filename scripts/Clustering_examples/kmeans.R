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
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

cat("\014")                     # clears all the variables 
rm(list=ls(all=TRUE))           # remove all variables of the workspace

# N <- 100
# x <- c(rnorm(100, mean = 0, sd = 0.3) ,rnorm(100, mean = 1, sd = 0.3), rnorm(100, mean = 2, sd = 0.3))
# df <- data.frame(x)
# df$y <- c(rnorm(100, mean = 0, sd = 0.3) ,rnorm(100, mean = 1, sd = 0.3), rnorm(100, mean = 2, sd = 0.3))
# 
# df_mat <- scale(df) # Scaling the data
# 
# km.res <- kmeans(df_mat, 3)
# print(km.res)
# 
# df$cluster <- km.res$cluster
# km.res$centers
# 
# centroid <- as.data.frame(km.res[[2]])
# 
# centroid$cluster <- c(1,2,3)
# 
# ggplot() + 
#   geom_point(data = df, mapping = aes(x,y, colour = as.factor(cluster))) + 
#   geom_point(data = centroid, mapping = aes(x,y))

# a 2-dimensional example
N <- 500
x <- rbind(matrix(rnorm(N, sd = 0.3), ncol = 2),
           matrix(rnorm(N, mean = 1, sd = 0.5), ncol = 2),
           matrix(rnorm(N, mean = 3, sd = 0.5), ncol = 2))
colnames(x) <- c("x", "y")

C1 <- c(1.1,2.2)
C2 <- c(0.2,-1)
C3 <- c(2.5,2.5)
centroid <- as.data.frame(rbind(C1,C2,C3))


cl <- kmeans(x, rbind(C1,C2,C3) )
#fviz_cluster(cl, x)
df <- data.frame(x)
df$cluster <- cl$cluster

centroid$x_new <- cl$centers[,1] 
centroid$y_new <- cl$centers[,2] 

centroid$cluster <- as.factor(c("Cluster 1","Cluster 2","Cluster 3"))

dev.new()
ggplot() + 
  geom_point(data = df, mapping = aes(x,y,colour = as.factor(cluster))) +
  # geom_point(data = centroid, mapping = aes(x_new,y_new), shape = 1, size = 3, stroke = 1)+
  # geom_point(data = centroid, mapping = aes(V1, V2), shape = 2, size = 3, stroke = 1) +
  theme_void()

