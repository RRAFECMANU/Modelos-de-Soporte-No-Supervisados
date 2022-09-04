#Ejemplo CLARA
set.seed(5678)
# Generamos 1,500,000 objetos, divididos en 2 clusters.

df <- rbind(cbind(rnorm(10000,0,8),rnorm(10000,0,8)),
            cbind(rnorm(10000,50,8), rnorm(10000,50,8)))


# Definimos un nombre para filas y columnas
colnames(df) <- c("x", "y")
rownames(df) <- paste0("S", 1:nrow(df))
# Inspeccionamos los datos generados
head(df, nrow = 6)

datos=data.frame(df)

setwd("C:/Users/roman.rodriguez/Documents/Cursos/Soporte No Supervisado") 

write.table(datos, file="Prueba.txt")



#Abrimos las librerías necesarias 

library(cluster)
library(factoextra)

#Identificamos el número de cluster a usar por el estadístico silhouette

fviz_nbclust(df, clara, method = "silhouette")+ theme_classic()

# Ejecutamos el algoritmo CLARA

clara.res <- clara(df, 2, samples = 50, pamLike = TRUE)

# Objetos generados como resultado del algoritmo
print(clara.res)


dd <- cbind(df, cluster = clara.res$cluster)
head(dd, n = 4)

# Medoids
clara.res$medoids

# Clustering
head(clara.res$clustering, 10)

#Para visualizar el cluster 

fviz_cluster(clara.res, palette = c("#00AFBB", "#FC4E07"), # definimos la paleta de colores
             ellipse.type = "t", # Dibujamos una elipse
             geom = "point", pointsize = 1,
             ggtheme = theme_classic()
            )



