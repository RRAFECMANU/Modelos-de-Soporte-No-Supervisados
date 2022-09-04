#Validación de agrupación

df <- iris[, -5]

random_df <- apply(df, 2,
                   function(x){runif(length(x), min(x), (max(x)))})
random_df <- as.data.frame(random_df)

df <- iris.scaled <- scale(df)
random_df <- scale(random_df)

library("factoextra")

# Graficamos la base original

fviz_pca_ind(prcomp(df), title = "PCA - Iris data",
             habillage = iris$Species, palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

# Graficamos la base generada
fviz_pca_ind(prcomp(random_df), title = "PCA - Random data",
             geom = "point", ggtheme = theme_classic())

#K-medias

set.seed(123)
# K-means base iris
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = 	FALSE, palette = "jco", ggtheme = 	theme_classic())


#K-medias base simulada
set.seed(123)
# K-means base iris
km.res1 <- kmeans(df, 3)
fviz_cluster(list(data = df, cluster = km.res1$cluster),
             ellipse.type = "norm", geom = "point", stand = 	FALSE, palette = "jco", ggtheme = 	theme_classic())


#Jerárquico aglomerativo en la base simulada

fviz_dend(hclust(dist(random_df)), k = 3, 	k_colors = "jco",
          as.ggplot = TRUE, show_labels = FALSE)


library(clustertend)
# Estimar el estadístico de Hopkins base original
set.seed(123)
hopkins(df, n = nrow(df)-1)
# Estimar el estadístico de Hopkins base simulada
set.seed(123)
hopkins(random_df, n = nrow(random_df)-1)




fviz_dist(dist(df), show_labels = FALSE)+
  labs(title = "Iris data")

fviz_dist(dist(random_df), show_labels = FALSE)	+labs(title = "Random data")












