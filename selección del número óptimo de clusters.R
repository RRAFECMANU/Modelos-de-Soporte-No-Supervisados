# Selecci?n del n?mero ?ptimo de clusters

pkgs <- c("factoextra", "NbClust")
install.packages(pkgs)


library(factoextra)
library(NbClust)

df <- scale(USArrests)
head(df)


# Elbow method

fviz_nbclust(df, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)+ labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 para mantener la función rápida.
# valor recomendado: nboot = 500 para su análisis.
# Use verbose = FALSE para ocultar las estimaciones.

set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")



#NbClust

library("NbClust")
nb <- NbClust(df, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")

library("factoextra")
fviz_nbclust(nb)


