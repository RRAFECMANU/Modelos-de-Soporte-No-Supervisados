
library(arules)
library(arulesViz)


#Lectura de datos

data("Groceries")

Groceries 

#Generación de las reglas 

rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.05))

#Inspección de las reglas 

inspect(head(sort(rules, by="lift"),7))

#Gráficos

plot(rules, measure=c("support", "confidence", shading="lift"), interective=FALSE)


#Generación de subgrupos

milk.rules <- sort(subset(rules, subset=rhs %in% "whole milk"), by="confidence")

summary(milk.rules)

inspect(head(sort(milk.rules, by="lift"),7))

plot(milk.rules, measure=c("support", "confidence"), shading="lift")


meat.rules <- sort(subset(rules, subset = lhs %in%  "beef"|lhs %in%  "sausage" |lhs %in%  "chicken"), by = "confidence")

summary(meat.rules)

inspect(head(sort(meat.rules), by="lift",7))


plot(meat.rules,method="graph",interactive=FALSE,shading="lift")



