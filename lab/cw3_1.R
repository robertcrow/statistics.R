# rle -> running length encoder
rle(c(1,1,1,2,2,2,1,3,2,2))
rle(c(1,1,1,2,2,2,1,3,2,2))->R
inverse.rle(R)
R$lengths[2]<-7

# parallel computing in R
library(parallel)
mclapply(1:8, function(x) - x)
Sys.getpid()
unlist(lapply(1:8, function(x) Sys.getpid()))

