#Ejemplo cluster datos mixtos
"A modo ilustrativo, se utilizará el conjunto de datos universitarios disponibles públicamente que se encuentran en el paquete ISLR, 
que tiene varias estadísticas de universidades estadounidenses desde 1995 (N = 777). Para resaltar el desafío de manejar tipos de 
datos mixtos, se usarán variables que son tanto categóricas como continuas y se enumeran a continuación:
Continuo
Nivel de aceptación
Matrícula fuera de la escuela
Número de nuevos estudiantes matriculados
Categórico
Si una universidad es pública / privada
Si una universidad es de élite, se define como que tiene más del 50% de estudiantes nuevos que se graduaron en el 10% superior de 
su clase de la escuela secundaria."



set.seed(1680) # for reproducibility

library(dplyr) # for data cleaning
library(ISLR) # for college dataset
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot
library(ggplot2) # for visualization

"Antes de que pueda comenzar la agrupación, se debe realizar una limpieza de datos:

La tasa de aceptación se crea al bucear el número de aceptaciones por el número de aplicaciones
isElite se crea etiquetando a las universidades con más del 50% de sus nuevos estudiantes que se encontraban en el 10% superior de
su clase de 
secundaria como elite"

college_clean <- College %>%
  mutate(name = row.names(.),
         accept_rate = Accept/Apps,
         isElite = cut(Top10perc,
                       breaks = c(0, 50, 100),
                       labels = c("Not Elite", "Elite"),
                       include.lowest = TRUE))  %>%
  mutate(isElite = factor(isElite))  %>%
  select(name, accept_rate, Outstate, Enroll,
         Grad.Rate, Private, isElite)

glimpse(college_clean)

## Observations: 777
## Variables: 7
## $ name        (chr) "Abilene Christian University", "Ad...
## $ accept_rate (dbl) 0.7421687, 0.8801464, 0.7682073, 0....
## $ Outstate    (dbl) 7440, 12280, 11250, 12960, 7560, 13...
## $ Enroll      (dbl) 721, 512, 336, 137, 55, 158, 103, 4...
## $ Grad.Rate   (dbl) 60, 56, 54, 59, 15, 55, 63, 73, 80,...
## $ Private     (fctr) Yes, Yes, Yes, Yes, Yes, Yes, Yes,...
## $ isElite     (fctr) Not Elite, Not Elite, Not Elite, E...

"Para que un algoritmo aún por elegir agrupe las observaciones, primero debemos definir alguna noción de (des) similitud entre 
las observaciones. Una opción popular para la agrupación es la distancia euclidiana. Sin embargo, la distancia euclidiana solo 
es válida para variables continuas, y por lo tanto no es aplicable aquí. Para que un algoritmo de agrupamiento produzca resultados 
razonables, debemos usar una métrica de distancia que pueda manejar tipos de datos mixtos. En este caso, usaremos algo llamado 
distancia de Gower."


"El concepto de distancia de Gower es bastante simple. Para cada tipo de variable, se usa una escala de medición particular que funciona bien para ese tipo y se escala para estar entre 0 y 1. Luego, se calcula una combinación lineal usando pesos especificados por el usuario (más simplemente un promedio) para crear la matriz de distancia final . Las métricas utilizadas para cada tipo de datos se describen a continuación:

cuantitativo (intervalo): distancia de Manhattan normalizada por rango
ordinal: la variable se clasifica primero, luego se usa la distancia de Manhattan con un ajuste especial para las relaciones
nominal: las variables de k categorías se convierten primero en k columnas binarias y luego se usa el coeficiente de dados

profesionales: Intuitivo de entender y sencillo de calcular
Contras: Sensible a la no normalidad y valores atípicos presentes en variables continuas, por lo que las transformaciones como 
un paso de preprocesamiento podrían ser necesarias. También se necesita calcular una matriz de distancia NxN, que es 
computacionalmente intensa para mantener en la memoria muestras grandes
A continuación, vemos que la distancia de Gower se puede calcular en una línea usando la función de margarita. 
Tenga en cuenta que debido a un sesgo positivo en la variable Enroll, una transformación de registro se realiza 
internamente a través del argumento de tipo. Las instrucciones para realizar transformaciones adicionales, como los 
factores que podrían considerarse como binarios asimétricos (como eventos raros), se pueden ver en? Daisy."

# Remove college name before clustering

gower_dist <- daisy(college_clean[, -1],
                    metric = "gower",
                    type = list(logratio = 3))

# Check attributes to ensure the correct methods are being used
# (I = interval, N = nominal)
# Note that despite logratio being called, 
# the type remains coded as "I"

summary(gower_dist)
## 301476 dissimilarities, summarized :
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0018601 0.1034400 0.2358700 0.2314500 0.3271400 0.7773500 
## Metric :  mixed ;  Types = I, I, I, I, N, N 
## Number of objects : 777

"Como un control de cordura, podemos imprimir el par más similar y diferente en los datos para ver si tiene sentido. 
En este caso, se considera que la Universidad de St. Thomas y la Universidad John Carroll son las más similares dadas 
las siete características utilizadas en el cálculo de distancia, mientras que la Universidad de Ciencias y Artes de Oklahoma 
y Harvard son las más diferentes."


gower_mat <- as.matrix(gower_dist)

# Output most similar pair

college_clean[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
##                            name accept_rate Outstate Enroll
## 682 University of St. Thomas MN   0.8784638    11712    828
## 284     John Carroll University   0.8711276    11700    820
##     Grad.Rate Private   isElite
## 682        89     Yes Not Elite
## 284        89     Yes Not Elite
# Output most dissimilar pair

college_clean[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
##                                        name accept_rate
## 673 University of Sci. and Arts of Oklahoma   0.9824561
## 251                      Harvard University   0.1561486
##     Outstate Enroll Grad.Rate Private   isElite
## 673     3687    208        43      No Not Elite
## 251    18485   1606       100     Yes     Elite

"Elegir un algoritmo de agrupamiento
Ahora que se ha calculado la matriz de distancia, es hora de seleccionar un algoritmo para agrupar. Si bien existen muchos 
algoritmos que pueden manejar una matriz de distancia personalizada, aquí se usará la partición alrededor de Medoids (PAM).

El particionamiento alrededor de medoids es un procedimiento de agrupamiento iterativo con los siguientes pasos:

Elige k entidades aleatorias para convertirte en los medoides
Asignar a cada entidad a su medoide más cercano (usando nuestra matriz de distancia personalizada en este caso)
Para cada grupo, identifique la observación que produciría la distancia promedio más baja si fuera reasignada como el
medoide. Si es así, haz de esta observación el nuevo medoide.
Si al menos un medoide ha cambiado, vuelva al paso 2. De lo contrario, finalice el algoritmo.
Si conoce el algoritmo k-means, esto podría parecer muy familiar. De hecho, ambos enfoques son idénticos, excepto que
k-means tiene centros de conglomerados definidos por distancia euclidiana (es decir, centroides), mientras que los centros 
de conglomerados para PAM están restringidos a ser las observaciones mismas (es decir, medoides).

pros: Fácil de entender, más robusto para el ruido y valores atípicos en comparación con k-means, y tiene el beneficio 
adicional de tener una observación como ejemplo para cada clúster
contras: tanto el tiempo de ejecución como la memoria son cuadráticos (es decir, $ O (n ^ 2) $)
Seleccionar la cantidad de clusters
Existe una variedad de métricas para ayudar a elegir la cantidad de conglomerados que se extraerán en un análisis de 
conglomerados. Utilizaremos el ancho de silueta, una métrica de validación interna que es una medida agregada de cuán similar
es una observación a su propio clúster en comparación con su clúster vecino más cercano. La métrica puede variar de -1 a 1, 
donde los valores más altos son mejores. Después de calcular el ancho de la silueta para clústeres que varían de 2 a 10 para 
el algoritmo PAM, vemos que 3 clústeres arrojan el valor más alto."

# Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)

"Interpretación de grupos
A través de estadísticas descriptivas
Después de ejecutar el algoritmo y seleccionar tres clusters, podemos interpretar los clústeres ejecutando un resumen 
en cada clúster. Con base en estos resultados, parece que el Cluster 1 es principalmente Privado / No Elite con niveles 
medios de matrícula fuera del estado y menores niveles de inscripción. El clúster 2, por otro lado, es principalmente privado 
/ Elite con menores niveles de aceptación, altos niveles de matrícula fuera del estado y altas tasas de graduación. Finalmente,
el grupo 3 es principalmente Público / No Elite con los niveles más bajos de matrícula, los mayores niveles de inscripción y
la tasa de graduación más baja."

pam_fit <- pam(gower_dist, diss = TRUE, k = 3)

pam_results <- college_clean %>%
  dplyr::select(-name) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary
## [[1]]
##   accept_rate        Outstate         Enroll      
##  Min.   :0.3283   Min.   : 2340   Min.   :  35.0  
##  1st Qu.:0.7225   1st Qu.: 8842   1st Qu.: 194.8  
##  Median :0.8004   Median :10905   Median : 308.0  
##  Mean   :0.7820   Mean   :11200   Mean   : 418.6  
##  3rd Qu.:0.8581   3rd Qu.:13240   3rd Qu.: 484.8  
##  Max.   :1.0000   Max.   :21700   Max.   :4615.0  
##    Grad.Rate      Private        isElite       cluster 
##  Min.   : 15.00   No :  0   Not Elite:500   Min.   :1  
##  1st Qu.: 56.00   Yes:500   Elite    :  0   1st Qu.:1  
##  Median : 67.50                             Median :1  
##  Mean   : 66.97                             Mean   :1  
##  3rd Qu.: 78.25                             3rd Qu.:1  
##  Max.   :118.00                             Max.   :1  
## 
## [[2]]
##   accept_rate        Outstate         Enroll      
##  Min.   :0.1545   Min.   : 5224   Min.   : 137.0  
##  1st Qu.:0.4135   1st Qu.:13850   1st Qu.: 391.0  
##  Median :0.5329   Median :17238   Median : 601.0  
##  Mean   :0.5392   Mean   :16225   Mean   : 882.5  
##  3rd Qu.:0.6988   3rd Qu.:18590   3rd Qu.:1191.0  
##  Max.   :0.9605   Max.   :20100   Max.   :4893.0  
##    Grad.Rate      Private       isElite      cluster 
##  Min.   : 54.00   No : 4   Not Elite: 0   Min.   :2  
##  1st Qu.: 77.00   Yes:65   Elite    :69   1st Qu.:2  
##  Median : 89.00                           Median :2  
##  Mean   : 84.78                           Mean   :2  
##  3rd Qu.: 94.00                           3rd Qu.:2  
##  Max.   :100.00                           Max.   :2  
## 
## [[3]]
##   accept_rate        Outstate         Enroll    
##  Min.   :0.3746   Min.   : 2580   Min.   : 153  
##  1st Qu.:0.6423   1st Qu.: 5295   1st Qu.: 694  
##  Median :0.7458   Median : 6598   Median :1302  
##  Mean   :0.7315   Mean   : 6698   Mean   :1615  
##  3rd Qu.:0.8368   3rd Qu.: 7748   3rd Qu.:2184  
##  Max.   :1.0000   Max.   :15516   Max.   :6392  
##    Grad.Rate      Private        isElite       cluster 
##  Min.   : 10.00   No :208   Not Elite:199   Min.   :3  
##  1st Qu.: 46.00   Yes:  0   Elite    :  9   1st Qu.:3  
##  Median : 54.50                             Median :3  
##  Mean   : 55.42                             Mean   :3  
##  3rd Qu.: 65.00                             3rd Qu.:3  
##  Max.   :100.00                             Max.   :3

"Otro beneficio del algoritmo PAM con respecto a la interpretación es que los medoides sirven como ejemplos de cada grupo. 
De esto, vemos que Saint Francis University es el medoid del clúster Privado / No Elite, Barnard College es el medoid para 
el clúster Privado / Elite, y Grand Valley State University es el medoid para el clúster Public / Not Elite."

college_clean[pam_fit$medoids, ]
##                              name accept_rate Outstate
## 492         Saint Francis College   0.7877629    10880
## 38                Barnard College   0.5616987    17926
## 234 Grand Valley State University   0.7525653     6108
##     Enroll Grad.Rate Private   isElite
## 492    284        69     Yes Not Elite
## 38     531        91     Yes     Elite
## 234   1561

"A través de la visualización
Una forma de visualizar muchas variables en un espacio de dimensiones más bajas es con la incrustación de vecindario estocástica t, 
o t-SNE. Este método es una técnica de reducción de dimensión que intenta preservar la estructura local para hacer que los 
conglomerados sean visibles en una visualización 2D o 3D. Si bien normalmente utiliza la distancia euclidiana, tiene la 
capacidad de manejar una medida de distancia personalizada como la que creamos anteriormente. En este caso, el gráfico muestra 
los tres clústeres bien separados que PAM pudo detectar. Una cosa curiosa a tener en cuenta es que hay un pequeño grupo que se 
divide entre el clúster de Private / Elite y el clúster de Public / Not Elite."

tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)

tsne_data <- tsne_obj$Y %>%
  data.frame()  %>%
  setNames(c("X", "Y"))  %>%
  mutate(cluster=factor(pam_fit$clustering),
         nname = college_clean$name)

ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))

"Al investigar más, parece que este grupo está formado por las escuelas públicas más grandes y competitivas, como la Universidad
de Virginia o la Universidad de California en Berkeley. Aunque no son lo suficientemente grandes como para garantizar un grupo 
adicional de acuerdo con el ancho de la silueta, estas 13 escuelas ciertamente tienen características distintas de las otras 
tres agrupaciones."


 tsne_data %>%

   filter(X > 15 & X < 25, 
          Y > -15 & Y < -10) %>%
   left_join(college_clean, by="name")  %>%
   collect %>%
   .[["name"]] 

##  [1] "College of William and Mary"                
##  [2] "Georgia Institute of Technology"            
##  [3] "SUNY at Binghamton"                         
##  [4] "SUNY College at Geneseo"                    
##  [5] "Trenton State College"                      
##  [6] "University of California at Berkeley"       
##  [7] "University of California at Irvine"         
##  [8] "University of Florida"                      
##  [9] "University of Illinois - Urbana"            
## [10] "University of Michigan at Ann Arbor"        
## [11] "University of Minnesota at Morris"          
## [12] "University of North Carolina at Chapel Hill"
## [13] "University of Virginia"




