library(cluster)

iris[,-5]->i
dist(i[1:5,]) -> dd
dist(i) -> ddd

pam(ddd,3)->p
plot(p)

pam(i,3)-> pp
plot(pp)

plot(iris, col=p$clustering)
table(iris$Species, p$clustering)

install.packages("randomForest")
library(randomForest)

randomForest(Species~., data=iris, proximity=TRUE)->m
plot(pam(m$proximity, 3))
plot(iris, col=pam(m$proximity,3)$clustering)
m

