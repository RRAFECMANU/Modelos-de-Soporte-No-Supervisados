#Fuzzy clustering

library(cluster)
df <- scale(USArrests) # Datos estandarizados 

res.fanny <- fanny(df, 2) # Estimación del cluster difuso con k=2
head(res.fanny$membership, 3) # Membership coefficients
res.fanny$coeff # Dunns coefficient

head(res.fanny$clustering) # Grupos

library(factoextra)

fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE, palette = "jco", ggtheme = theme_minimal(), legend = "right")

fviz_silhouette(res.fanny, palette = "jco", ggtheme = theme_minimal())

#library(factoextra)
#fviz_nbclust(df, fanny, method = "wss") + geom_vline(xintercept = 5, linetype = 2)+ labs(subtitle = "Elbow method")
