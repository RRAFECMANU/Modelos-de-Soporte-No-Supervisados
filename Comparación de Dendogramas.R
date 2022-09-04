# Comparación de Dendogramas

df<- scale(USArrests)

set.seed(234235)
ss<-simple(1:50, 10)


library(dendextend)

# Estimamos la matriz de distancias


res.dist <- dist(df, method = "euclidean")


# Generamos los dos análisis de cluster jerárquico aglomerativo

hc1 <- hclust(res.dist, method = "average")
hc2 <- hclust(res.dist, method = "ward.D2")



# Generamos los respectivos dendogramas

dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

# Generamos una lista que contenta los dos dendogramas 

dend_list <- dendlist(dend1, dend2)



tanglegram(dend1, dend2)
tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common 	branches
           main = paste("entanglement =", round(entanglement(dend_list), 	2))
          )

# Matriz de correlaciones Cophenetic
cor.dendlist(dend_list, method = "cophenetic")

# Matriz de correlación de Baker
cor.dendlist(dend_list, method = "baker")

# Coeficiente de correlación Cophenetic
cor_cophenetic(dend1, dend2)

# Coeficiente de correlación de Baker
cor_bakers_gamma(dend1, dend2)


# Crear multiples dendogramas 
dend1 <- df %>% dist %>% hclust("complete") %>% as.dendrogram
dend2 <- df %>% dist %>% hclust("single") %>% as.dendrogram
dend3 <- df %>% dist %>% hclust("average") %>% as.dendrogram
dend4 <- df %>% dist %>% hclust("centroid") %>% as.dendrogram



# Matriz de correlaciones

dend_list <- dendlist("Complete" = dend1, "Single" = dend2,
                      "Average" = dend3, "Centroid" = dend4)
cors <- cor.dendlist(dend_list)

# Analizamos matriz de correlaciones 
round(cors, 2)












