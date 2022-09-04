#P-value para cluster jerárquico

install.packages("pvclust")

library(pvclust)

#abrimos los datos 

data("lung")

head(lung[, 1:4])

#Dimensión de los datos

dim(lung)

#Generamos una muestra 

set.seed(123)
ss <- sample(1:73, 30) # extract 20 samples out of
df <- lung[, ss]

#Usamos la función pvclust

library(pvclust)
set.seed(123)
res.pv <- pvclust(df, method.dist="cor",
                  method.hclust="average", nboot = 10)

# Graficamos

plot(res.pv, hang = -1, cex = 0.5)
pvrect(res.pv)


#Extraemos los objetos de los clusters significativos

clusters <- pvpick(res.pv)


#Es posible realizar estimaciones paralelas

library(parallel)
cl <- makeCluster(2, type = "PSOCK")

# pvclust
res.pv <- parPvclust(cl, df, nboot=1000)
stopCluster(cl)


