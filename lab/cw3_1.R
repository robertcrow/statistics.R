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

unlist(mclapply(1:12, function(x){Sys.sleep(x%%4); Sys.getpid() }, mc.cores=4))
system.time(unlist(mclapply(1:12, function(x){Sys.sleep(x%%4); Sys.getpid() }, mc.cores=4))->w)
matrix(w,4)
system.time(unlist(mclapply(1:12, function(x){Sys.sleep(x%%4); Sys.getpid() }, mc.cores=4, mc.preschedule=FALSE))->w)
system.time(unlist(mclapply(1:12, function(x){Sys.sleep(x%%4); Sys.getpid() }, mc.cores=4, mc.preschedule=TRUE))->w)
matrix(w,4)

# GlobalEnv - using global variables

a<-3
ls()
.GlobalEnv[["a"]]
(function(){a<-9})() #doesn't change a variable, a is a local var
a
(function(){.GlobalEnv[["a"]]<-9})() # changes a variable
a
a = 1:10;
unlist(mclapply(1:12, function(x){.GlobalEnv[["a"]]<-2}))
a
unlist(mclapply(1:12, function(x){.GlobalEnv[["a"]]<-10:1})) 



# cluster
#install.packages('snow')
library(snow)

a <- 3
a
klaster <- makeCluster(rep("localhost", 4), type="SOCK")
klaster

clusterEvalQ(klaster, "a")
clusterExport(klaster,"a")
clusterEvalQ(klaster, a)

system.time(matrix(unlist(clusterApply(klaster, 1:12, function(x){Sys.sleep(x%%4); Sys.getpid()}),4)->w))
matrix(w,4)
system.time(matrix(unlist(clusterApplyLB(klaster, 1:12, function(x){Sys.sleep(x%%4); Sys.getpid()}),4)->w))


# Machine learning materials
#install.packages("rFerns")
library(rFerns)

install.packages("mlbench")
library(Sonar)
