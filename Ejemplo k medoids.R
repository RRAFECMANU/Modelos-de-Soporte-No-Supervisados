#K-medoids
data("USArrests") # Cargamos los datos
df <- scale(USArrests) # Estandarizamos
head(df, n = 3) # Muestra las primeras tres filas
library(cluster)
library(factoextra)
fviz_nbclust(df, pam, method = "silhouette")+
  theme_classic()
pam.res <- pam(df, 2)
print(pam.res)
dd <- cbind(USArrests, cluster = pam.res$cluster)
head(dd, n = 3)
pam.res$medoids
head(pam.res$clustering)
fviz_cluster(pam.res, palette = c("#00AFBB", "#FC4E07"), # Paleta de colores
             ellipse.type = "t", # Elipse de concentración
             repel = TRUE, # Evita el overplotting label
             ggtheme = theme_classic()
             )
