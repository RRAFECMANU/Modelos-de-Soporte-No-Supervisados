#Ejemplo de SOM en R

library("kohonen")

data(wines)
wines.sc <- scale(wines)
set.seed(777)

#Generación del SOM
wine.som <- som(wines.sc, grid=somgrid(15,10,"hexagonal"))

#Descriptores del SOM

names(wine.som)

summary(wine.som)

wine.som$grid

wine.som$codes


#Celdas correspondientes para cada individuo
wine.som$unit.classif


#Número de instancias asignadas a cada nodo

nb <- table(wine.som$unit.classif)

print(nb)


#Evaluamos si hay nodos vacíos

print(length(nb))



############################
#Representación gráfica

#Progresión del proceso de aprendizaje

#Gráfico de la influencia de las variables, codebook vectors, representa el vecgtor de pesos en un histograma para cada celda del mapa

plot(wine.som, main = "Datos de Vinos")

#Para analizar a detalle la influencia de las variables en cada celda podemos ver a detalle los valores
#de los codebooks

print(wine.som$codes)


#Counts plot: numero de instancias dentro de las celdas para identeificar áreas con alta densidad


degrade.bleu <-function(n){
  return(rgb(0,0.4,1,alpha=seq(0,1,1/n)))}

plot(wine.som,type="count",palette.name=degrade.bleu)

# Gráfica Neighbour distance plot (U-matrix) matriz unificada de distancias SOM con la distancia euclideana 
#entre los vectores codebook y los neighboring neurons. 


plot(wine.som, type="dist.neighbours")



# Clustering de los nodos


dc <- dist(wine.som$codes[[1]])


cah  <-hclust(dc,method="ward.D2")

plot(cah,hang=-1,labels=F)

rect.hclust(cah,k=3)

groupes <-cutree(cah, k=3)

print(groupes)

plot(wine.som,type="mapping",bgcol=c("steelblue1","sienna1","yellowgreen")[groupes])

add.cluster.boundaries(wine.som,clustering=groupes)


#Clustering de los individuos


ind.groupe <-groupes[wine.som$unit.classif]

print(ind.groupe)




#Base de datos de 150 plantas con cuatro características de las plantas: longitud del sépalo
#ancho del sépalo, longitud del pÃ©talo y ancho del pétalo. Hay 50 ejemplares para cada tipo de planta del 
#género iris: setosa, versicolor y virginica. 

#Queremos comprobar si un modelo SOM es capaz de agrupar los tres tipo de planta utilizando Ãºnicamente las cuatro variables citadas. 

#Usamos como criterio de similitud distancia ecuclideana y utilizamos un mapa bidimensional de 10x10

# Elaboraremos el modelo en dos etapas
# 1. Entrenamiento con un atasa de aprendizaje alta igual a 1 y un radio de vecindad igual al diámetro del mapa (10)
# 2. Conforme avanza el aprendizaje la tasa de aprendizaje y el radio de vecindad se reducen de forma lineal hata 0.05 y 1. 


library(datasets)
help(iris)



