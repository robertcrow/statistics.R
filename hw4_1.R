step <- function(a){a <- sample(c(-1,1), 1,replace=TRUE)}


tmp <- step()

k_tries <-function(displacements){

  N = 100
  n_steps = sapply(c(rep(0,N)), step)
  displacements <- sum(n_steps)
  return (displacements)
  
}

displacements = sapply(c(rep(0,250)), k_tries)
mean_displacement = mean(displacements)
 #cumsum                         
