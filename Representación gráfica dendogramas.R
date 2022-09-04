#Diferentes representaciones de Dendogramas

# Cargar datos
data(USArrests)
# Estimamos las distancias y agrupación jerárcica 
dd <- dist(scale(USArrests), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
install.packages(c("factoextra", "dendextend"))
install.packages("ggplot2")

library(ggplot2)
library(factoextra)

#Gráfico 1

fviz_dend(hc, cex = 0.5)


#Gráfico 2


fviz_dend(hc, cex = 0.5,
          main = "Dendrogram - ward.D2",
          xlab = "Objects", ylab = "Distance", sub = "")

fviz_dend(hc, cex = 0.5, horiz = TRUE)


#Gráfico 3

fviz_dend(hc, k = 4, # Corte en cuatro grupos
          cex = 0.5, # tamaño de la etiqueta
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # colorear cada etiqueta por grupos
          rect = TRUE, # Agrega un rectángulo a los grupos
          rect_border = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          rect_fill = TRUE)

#Gráfico 4

fviz_dend(hc, k = 4, # Corte en cuatro grupos
          cex = 0.5, # tamaño de las etiquetas
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # colorea etiquetas por grupos
          ggtheme = theme_gray() # Cambia el tema 
          )

#Gráfico 5


fviz_dend(hc, cex = 0.5, k = 4, # Corte en cuatro grupos
          k_colors = "jco")

fviz_dend(hc, k = 4, cex = 0.4, horiz = TRUE, k_colors = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

fviz_dend(hc, cex = 0.5, k = 4,
          k_colors = "jco", type = "circular")

#Gráfico 6 

install.packages("igraph")


require("igraph")
fviz_dend(hc, k = 4, k_colors = "jco",
          type = "phylogenic", repel = TRUE)



require("igraph")
fviz_dend(hc, k = 4, # Corte en cuatro grupos
          k_colors = "jco",
          type = "phylogenic", repel = TRUE,
          phylo_layout = "layout.gem")



# Crea una gráfica de todo el dendograma y un extracto de un dendograma


fviz_dend(hc, xlim = c(1, 20), ylim = c(1, 8))


dend_plot <- fviz_dend(hc, k = 4, # Corte en cuatro grupos
                       cex = 0.5, # label size
                       k_colors = "jco"
                        )

dend_data <- attr(dend_plot, "dendrogram") # Extracto de un dendograma



# Corte del dendograma en h = 10
dend_cuts <- cut(dend_data, h = 10)

# Visualizar la versión truncada 
fviz_dend(dend_cuts$upper)



# Graficar el dendograma completo
print(dend_plot)

# Graficar sub-árbol 1
fviz_dend(dend_cuts$lower[[1]], main = "Subtree 1")

# Graficar sub-árbol 2
fviz_dend(dend_cuts$lower[[2]], main = "Subtree 2")

#Dendograma circular
fviz_dend(dend_cuts$lower[[2]], type = "circular")



#Guardar como imagen pdf
pdf("dendrogram.pdf", width=30, height=15) # Abre un pdf
p <- fviz_dend(hc, k = 4, cex = 1, k_colors = "jco" ) # Grafica
print(p)
dev.off() # Cierra pdf



data <- scale(USArrests)
dist.res <- dist(data)
hc <- hclust(dist.res, method = "ward.D2")
dend <- as.dendrogram(hc)
plot(dend)



library(dendextend)
dend <- USArrests[1:5,] %>% # datos
  scale %>% # Estandarizar
  dist %>% # Matriz de distancias
  hclust(method = "ward.D2") %>% # Cluster jerárquico
  as.dendrogram # Generar dendograma
plot(dend)




