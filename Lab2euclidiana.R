library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(Rtsne) 
library(ggplot2) 
library(ggpubr)
set.seed(1111)
setwd("C://Users//Maxi//Desktop//semestre8//analisis_datos//Lab 2")
data<-read.csv("car.data",header = FALSE)
colnames(data)<-c("buying", "maint", "doors", "persons", "lug_boot","safety","class")
#Cambiar el tipo de datos de las variables a factor
data$buying<-factor(data$buying)
data$maint<-factor(data$maint)
data$doors<-factor(data$doors)
data$persons<-factor(data$persons)
data$lug_boot<-factor(data$lug_boot)
data$safety<-factor(data$safety)
data$class<-factor(data$class)

data.dummy <- cbind(model.matrix(class~buying-1,data=data), #Dicomtomizar las variables
             model.matrix(class~maint-1,data=data),
             model.matrix(class~doors-1,data=data),
             model.matrix(class~persons-1,data=data),
             model.matrix(class~lug_boot-1,data=data),
             model.matrix(class~safety-1,data=data),
             model.matrix(~class-1,data=data)
             )
data.dummy<-as.data.frame(data.dummy)

distancias<-get_dist(data.dummy,method = "euclidean")
fviz_nbclust(data.dummy,kmeans,method = "wss")
fviz_nbclust(data.dummy,kmeans,method = "silhouette")
#fviz_nbclust(data.dummy,kmeans,method = "gap_stat")

# k=2 k=3 k=5 k7 y k=9
k.definidos<-c(2,3,5,7,9)

k.medias<-kmeans(data.dummy,2)
cordenadas.distancias<- Rtsne(distancias, is_distance = TRUE)

cordenadas.distancias.data <- cordenadas.distancias$Y %>%
    data.frame() %>%
    setNames(c("X", "Y"))
  
cluster<-factor(k.medias$cluster)
cordenadas.distancias.data<-cbind( cordenadas.distancias.data,cluster)
  
g1<-ggplot(aes(x = X, y = Y), data = cordenadas.distancias.data) +
  geom_point(aes(color = cluster))+
  ggtitle ("Cluster k=2")

k.medias<-kmeans(data.dummy,3)
cordenadas.distancias<- Rtsne(distancias, is_distance = TRUE)

cordenadas.distancias.data <- cordenadas.distancias$Y %>%
  data.frame() %>%
  setNames(c("X", "Y"))

cluster<-factor(k.medias$cluster)
cordenadas.distancias.data<-cbind( cordenadas.distancias.data,cluster)

g2<-ggplot(aes(x = X, y = Y), data = cordenadas.distancias.data) +
  geom_point(aes(color = cluster))+
  ggtitle ("Cluster k=3")

k.medias<-kmeans(data.dummy,5)
cordenadas.distancias<- Rtsne(distancias, is_distance = TRUE)

cordenadas.distancias.data <- cordenadas.distancias$Y %>%
  data.frame() %>%
  setNames(c("X", "Y"))

cluster<-factor(k.medias$cluster)
cordenadas.distancias.data<-cbind( cordenadas.distancias.data,cluster)

g3<-ggplot(aes(x = X, y = Y), data = cordenadas.distancias.data) +
  geom_point(aes(color = cluster))+
  ggtitle ("Cluster k=5")

k.medias<-kmeans(data.dummy,7)
cordenadas.distancias<- Rtsne(distancias, is_distance = TRUE)

cordenadas.distancias.data <- cordenadas.distancias$Y %>%
  data.frame() %>%
  setNames(c("X", "Y"))

cluster<-factor(k.medias$cluster)
cordenadas.distancias.data<-cbind( cordenadas.distancias.data,cluster)

g4<-ggplot(aes(x = X, y = Y), data = cordenadas.distancias.data) +
  geom_point(aes(color = cluster))+
  ggtitle ("Cluster k=7")

k.medias<-kmeans(data.dummy,9)
cordenadas.distancias<- Rtsne(distancias, is_distance = TRUE)

cordenadas.distancias.data <- cordenadas.distancias$Y %>%
  data.frame() %>%
  setNames(c("X", "Y"))

cluster<-factor(k.medias$cluster)
cordenadas.distancias.data<-cbind( cordenadas.distancias.data,cluster)

g5<-ggplot(aes(x = X, y = Y), data = cordenadas.distancias.data,la) +
  geom_point(aes(color = cluster))+
  ggtitle ("Cluster k=9")

ggarrange(g1,g2,g3,g4,g5,
          ncol = 2, nrow = 3)



obtenerResumen<-function(data,pam){
  pam_results <- data %>%
    mutate(cluster = factor(pam$cluster)) %>%
    group_by(cluster) %>%
    do(the_summary = summary(.))
  pam_results$the_summary
  
}


