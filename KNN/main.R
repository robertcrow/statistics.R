# library("mlbench", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
# rows are parameters of an individual observation
data(Sonar)

sonar_class = Sonar$Class
ds_dim = dim(Sonar)
B = matrix(Sonar[1 : ds_dim[1], 1 : ds_dim[2] - 1 ], ds_dim[2] -1 , ds_dim[1])
B <- rownames(c(1:60))
euclidian_norm <- function(vecs, ref_vec) { lapply(vecs, sqrt(sum((vecs - ref_vec)^2 ))) }

dist = euclidian_norm(B[1,], B[c(2:10),])
a = 2:10

B[1,]
a= unlist(B[c(2:4),])
