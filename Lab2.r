library(dplyr)
library(tidyr)
library(cluster)
library(factoextra)
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization
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

#Establecer matriz de distancias usando la distancia de gower
distancias<-daisy(data,metric = "gower",type = list(logratio = 3))
distancias.matriz<-as.matrix(distancias)


#Estimar la cantidad de cluster a utilizar
#fviz_nbclust(distancias.matriz,pam,method = "wss")
#fviz_nbclust(distancias.matriz,pam,method = "silhouette")

#Por temas de rendimiento computacional no se realiza el analisis estimativo con
# gap_stat

#Se revisan 2 k distintos, obtenidos de los analisis estimativos
# Del metodo del codo se obtienen k=2 y k=5
# Mientras que el optimo del metodo de la silueta es k=2
pam.k2<-pam(distancias.matriz, k = 2)
pam.k5<-pam(distancias.matriz, k = 5)

#Graficar cluster de pam con k=5
cordenadas.distancias.k5<- Rtsne(distancias, is_distance = TRUE)

cordenadas.distancias.k5.data <- cordenadas.distancias.k5$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam.k5$clustering))

ggplot(aes(x = X, y = Y), data = cordenadas.distancias.k5.data) +
  geom_point(aes(color = cluster))


#Graficar cluster de pam con k=2
cordenadas.distancias.k2<- Rtsne(distancias, is_distance = TRUE)

cordenadas.distancias.k2.data <- cordenadas.distancias.k2$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam.k2$clustering))

ggplot(aes(x = X, y = Y), data = cordenadas.distancias.k2.data) +
  geom_point(aes(color = cluster))



obtenerResumen<-function(data,pam){
  pam_results <- data %>%
  mutate(cluster = factor(pam$clustering)) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
  pam_results$the_summary

}

frecuenciaPorcentaje<-function(variable1,variable2,margen){
  if (!is.null(variable2)){
    tabla.frecuencia<-table(variable1,variable2)
    tabla.proporcion<-round(prop.table(tabla.frecuencia, margin = margen)*100,2)
    
    #Copiar la tabla de frecuencia 
    aux<-tabla.frecuencia
    for (i in 1:length(tabla.frecuencia)) {
      aux[i]<-paste(tabla.frecuencia[i]," (",tabla.proporcion[i],"%)",sep = "")
    }
    
    return(aux)
  }
  else{
    tabla.frecuencia<-table(variable1,dnn = c("Clase", "Grupo"))
    tabla.proporcion<-round(prop.table(tabla.frecuencia)*100,2)
    
    #Copiar la tabla de frecuencia 
    aux<-tabla.frecuencia
    for (i in 1:length(tabla.frecuencia)) {
      aux[i]<-paste(tabla.frecuencia[i]," (",tabla.proporcion[i],"%)",sep = "")
    }
    
    return(aux)
    
    
  }
}




obtenerResumen(data,pam.k2)
frecuenciaPorcentaje(data$class,pam.k2$clustering,margen = 1)
frecuenciaPorcentaje(data$class,pam.k5$clustering,margen = 1)

write.csv(frecuenciaPorcentaje(data$class,pam.k2$clustering,margen = 1),"clusterGowerk2.csv")
write.csv(frecuenciaPorcentaje(data$class,pam.k5$clustering,margen = 1),"clusterGowerk5.csv")

data.grupo1<-cbind(data,pam.k2$clustering)%>%filter(pam.k2$clustering == 1)

obtenerResumen(data,pam.k5)
