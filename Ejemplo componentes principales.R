USArrests

states=row.names (USArrests)
states
names (USArrests)
apply (USArrests, 2, mean)  
apply (USArrests, 2, var)
cor(USArrests[,1:4])
plot(USArrests[,1:4], pch='*',col=c("red", "green", "blue")[unclass(iris[,5])])
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
biplot(pr.out, scale=0)
pr.out$rotation=-pr.out$rotation 
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.var=pr.out$sdev^2
pr.var
summary(pr.out)
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab=" Principal Component ", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum (pve), xlab="Principal Component", ylab ="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
cor(USArrests[,1:4])
plot(USArrests[,1:4], pch='*',col=c("red", "green", "blue")[unclass(iris[,5])])
summary(pr.out)
pr.out$x
