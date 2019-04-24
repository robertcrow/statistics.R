#library("mlbench", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
# rows are parameters of an individual observation
data(Sonar)

# pre-processing

sonar_class = Sonar$Class
ds_dim = dim(Sonar)
B = data.matrix(Sonar)
B = B[, c(1:ds_dim[2]- 1)]

# supplementary tools for distance computation

euclidian_norm <- function(vecs, ref_vec) {sqrt(sum((vecs - ref_vec)^2 ))}

dists <- function(x, ref_row) {
  
  ref_row = 1    
  ref_vec = x[ref_row, ]
  vecs = x[-ref_row, ]
  
  dists = apply(vecs, 1, FUN=euclidian_norm, ref_vec)
  return (dists)
  
}

# actual KNN

kNNsingle <- function(a, B, Y, k){

  classes = levels(Y)
  dists = dists(B, a)
  match_ind = order(dists, decreasing=FALSE)[1:k]
  match_class = Y[match_ind]
  decision = which.max(table(match_class))
  result = classes[decision]
  
  return (result)
  
}

kNN <- function(trainX, trainY, testX, k){
  
  mine_or_rock = apply(X=testX, MARGIN=1, FUN=kNNsingle, trainX=trainX, trainY=trainY, k=k)
  return (mine_or_rock)
  
}


