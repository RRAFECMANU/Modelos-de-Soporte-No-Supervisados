# Selección del mejor algoritmo a utilizar

library(clValid)
# Iris (base de datos)

# Quitamos columna especies y estandarizamos
df <- scale(iris[, -5])


# Validación interna 

clmethods <- c("hierarchical","kmeans","pam")

intern <- clValid(df, nClust=2:6, clMethods=clmethods, validation="internal")

# Resumen
summary(intern)


# Medidas de estabilidad

clmethods <- c("hierarchical","kmeans","pam")

stab <- clValid(df, nClust = 2:6, clMethods = clmethods,
                validation = "stability")

# Scores óptimos 
optimalScores(stab)

#rownames(df) <- 1:150