library(factoextra)
data("multishapes")
df <- multishapes[, 1:2]
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df, geom = "point",
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())


install.packages("fpc")
install.packages("dbscan")


# Abrimos los datos multishapes
data("multishapes", package = "factoextra")
df <- multishapes[, 1:2]

# Estimamos DBSCAN
library("fpc")
set.seed(123)
db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)


# Graficamos los resultados
library("factoextra")
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())


print(db)


# Miembros del Cluster, los outliers son codificacdos como 0
# Muestra aleatoria 
db$cluster[sample(1:1089, 20)]


dbscan::kNNdistplot(df, k = 5)
abline(h = 0.15, lty = 2)




