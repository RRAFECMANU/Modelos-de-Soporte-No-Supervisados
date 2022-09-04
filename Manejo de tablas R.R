#Inspecci?n de una tabla

iris
print(iris)
plot(iris)
summary(iris)
str(iris)
head(iris)
tail(iris)
?summary
dim(iris)
nrow(iris)
ncol(iris)
colnames(iris)

#Selecci?n de filas y columnas 

iris[1:10]
iris[, 3:4]
iris[1:10, 3:4]

iris[, "species"]
iris$Species
iris[iris$Species == "setosa",]


# Creaci?n y eliminaci?n de tablas (o columnas)

copia.iris <- iris 
ls()
rm(copia.iris)
ls()

mi.iris <- iris
mi.iris$Petal.Area <- mi.iris$Petal.Length * mi.iris$Petal.Width
mi.iris$Petal.Area <- NULL

#Ordenaci??n
mi.iris <- iris[order(iris$Petal.Length),]

# Directorio de trabajo 

getwd()
setwd("c:/users/yo/proyecto")
dir()   

#Leer CSV

datos <- read.table("data_dir/mi_fichero.csv", sep = "\t", header = TRUE)


#Gr??ficos b??sicos

hist(iris$Sepal.Width)

hist(iris$Sepal.Width, main = "iris: histograma de la anchura de los s??palos",
     xlab = "anchura del s??palo", ylab = "frecuencia",
     col = "steelblue")

barplot(table(iris$Species))


barplot(VADeaths[, 2], xlab = "tramos de edad", ylab = "tasa de mortalidad",
        main = "Tasa de mortalidad en Virginia\nmujer/rural")

plot(cars$speed, cars$dist)


plot(airquality$Temp, type = "l")

plot(airquality$Temp)
lines(airquality$Temp)

plot(airquality$Temp)
lines(airquality$Temp)
abline(h = mean(airquality$Temp), col = "red")


boxplot(iris$Sepal.Width ~ iris$Species, col = "gray",
        main = "Especies de iris\nseg??n la anchura del s??palo")

#Vectores

table(iris$Species)

z <- table(iris$Species)
z["setosa"]

z[c("setosa", "virginica")]














