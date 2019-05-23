#install.packages("infotheo")
library(infotheo)

infotheo::entropy(sample(c(T,F),10000,replace=TRUE))
infotheo::entropy(sample(c(T,F),10000,replace=TRUE))/log(2)
infotheo::entropy(sample(c(T,F),10000, prob=c(0.05,0.95)))/log(2)

infotheo::multiinformation(c(T,F), c(T,F)) /log(2)
infotheo::multiinformation(c(T,T,F,F), c(T,F,F,T)) /log(2)

expand.grid(A=c(T,F), B=c(F,T),C=c(F,T))->X
X
Y<-X$A&X$B
Y
