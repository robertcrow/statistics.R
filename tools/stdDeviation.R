stdDeviation <- function(X){
  
  mean_value = sum(X) / length(X)
  var = (X - mean_value)^2
  dev = sqrt(var)  
  
  return(dev)
  
}