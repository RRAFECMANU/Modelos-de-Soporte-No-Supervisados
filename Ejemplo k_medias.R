#Ejemplo K-medias
set.seed (2)
x=matrix (rnorm (50*2) , ncol =2)
x[1:25 ,1]=x[1:25 ,1]+3
x[1:25 ,2]=x[1:25 ,2] -4
km.out =kmeans (x,2, nstart =20)
km.out
km.out$cluster
plot(x, col =(km.out$cluster +1) , main="K-Means Clustering Results with K=2", xlab ="", ylab="", pch =20, cex =2)


# Usando k=3
set.seed (4)
km.out =kmeans (x,3, nstart =20)
km.out
km.out$centers
km.out$cluster
km.out$totss
km.out$withinss
km.out$tot.withinss
km.out$betweenss
km.out$size
km.out$iter
km.out$ifault
plot(x, col =(km.out$cluster +1) , main="K-Means Clustering Results with K=3", xlab ="", ylab="", pch =20, cex =2)



