#Cluster jerárquico con K-medias


# Estandarizamos

df <- scale(USArrests)

#Estimación del cluster jerárquico con k-medias

library(factoextra)
res.hk <-hkmeans(df, 4)


#Objetos generados

names(res.hk)


#Imprimimos los resultados

res.hk


#Graficamos el dendograma

fviz_dend(res.hk, cex = 0.6, palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)


# Graficamos el cluster final 

fviz_cluster(res.hk, palette = "jco", repel = TRUE,
             ggtheme = theme_classic())




