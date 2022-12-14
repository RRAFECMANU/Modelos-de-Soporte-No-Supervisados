# Validaci?n de clusters

install.packages(c("factoextra", "fpc", "NbClust"))

library(factoextra)
library(fpc)
library(NbClust)

# Exclu?mos la variable especies (columna 5)
df <- iris[, -5]

# Estandarizando
df <- scale(df)

# K-medias 
km.res <- eclust(df, "kmeans", k = 3, nstart = 25, graph = FALSE)

# Gr?fica K-medias
fviz_cluster(km.res, geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())


# Cluster Jer?rquico

hc.res <- eclust(df, "hclust", k = 3, hc_metric = "euclidean",hc_method = "ward.D2", graph = FALSE)

# Gr?fica del Dendograma
fviz_dend(hc.res, show_labels = FALSE,
          palette = "jco", as.ggplot = TRUE)

######Estad?stico Silhouette

fviz_silhouette(km.res, palette = "jco",
                ggtheme = theme_classic())


# Informaci?n del estad?stico Silhouette
silinfo <- km.res$silinfo
names(silinfo)

# Silhouette widths (ancho) de cada observaci?n
head(silinfo$widths[, 1:3], 10)

# Average silhouette width (ancho de la silueta promedio) para cada cluster
silinfo$clus.avg.widths

#  Promedio total (promedio de todos los anchos)
silinfo$avg.width


# El tama?o de cada cluster 
km.res$size


# Silhouette width (ancho-amplitud) de la observaci?n
sil <- km.res$silinfo$widths[, 1:3]

# Objetos con Silhouette negativo

neg_sil_index <- which(sil[,'sil_with']<0)
sil[neg_sil_index, , drop=FALSE]


##### ?ndice de Dunn 

library(fpc)

# Estad?siticas de un cluster generado por K-medias
km_stats <- cluster.stats(dist(df), km.res$cluster)

# ?ndice de Dunn 
km_stats$dunn


###Validaci?n Externa


table(iris$Species, km.res$cluster)

library("fpc")

# Estad?sticas del cluster 
species <- as.numeric(iris$Species)
clust_stats <- cluster.stats(d = dist(df),
                             species, km.res$cluster)

# ?ndice de Rand corregido
clust_stats$corrected.rand

# ?ndice VI Meila 
clust_stats$vi

# Co?ncidencias entre la variable especies y el cluster genrado por PAM
pam.res <- eclust(df, "pam", k = 3, graph = FALSE)
table(iris$Species, pam.res$cluster)
cluster.stats(d = dist(iris.scaled),
              species, pam.res$cluster)$vi


# Co?ncidencias entre la variable especies y el cluster Jer?rquico Aglomerativo
res.hc <- eclust(df, "hclust", k = 3, graph = FALSE)
table(iris$Species, res.hc$cluster)
cluster.stats(d = dist(iris.scaled),
              species, res.hc$cluster)$vi



                           
                           
                           