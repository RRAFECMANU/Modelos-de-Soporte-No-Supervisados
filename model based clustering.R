#Model Based Clustering

library("mclust")
data ("diabetes")
head(diabetes,3)

df<-scale (diabetes[, -1])
mc<-Mclust (df) # Model based clustering
summary (mc)

library(factoextra) 
#Criterio BIC para seleccionar el número de clusters
fviz_mclust (mc , "BIC", palette ="jco") 
#Gráfica de los clusters
fviz_mclust (mc , "classification", geom = "point", pointsize = 1.5, palette="jco")
#Datos con incertidumbre en la clasificación
 fviz_mclust (mc, "uncertainty", palette = "jco")