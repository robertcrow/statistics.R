fun1 <- function(A) {
  
  dims = dim(A)
  rows = sample(c(1:dims[1]), dims[1], replace=TRUE)
  return (rows)
  
}

fun2 <- function(A, A_class, rows, k) {
  
  predictions = kNN(A[rows, ],  A_class[rows], A[-rows, ], k)
  return (predictions)
  
}


CV632 <- function(score, A, A_class, k) {
  
  rows = fun1(A)
  predictions = fun2(A, A_class, rows, k)
  score = sum(predictions == sonar_class[-rows]) / length(predictions)
  return(score)
}

CV632_test <-function(k, A, A_class){
  
  score = rep(0,30)
  average_score = mean(sapply(score, FUN=CV632, A=A, A_class=A_class, k=k))
  return (average_score)

}


k_factors = seq(1,10,1)
k_scores = sapply(k_factors, FUN=CV632_test, A=B, A_class=sonar_class)

boxplot(k_scores, notch=TRUE)
