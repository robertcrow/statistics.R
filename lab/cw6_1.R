library(cluster)
iris[,-5]->i
head(i)
dist(i[1:5,]) -> dd
dd
plot(i)
plot(i, col=iris$Species)
dist(i) -> ddd
agnes(dd)->a
plot(a)

agnes(ddd, method="single")->a
plot(a)

agnes(ddd, method="complete")->a
plot(a)


