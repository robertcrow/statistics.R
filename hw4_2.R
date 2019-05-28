N = c(100,500,1000,2000)

votes <- function(N){
  
  d = 0
  p = 0
  while (p < 0.95){
    
    d = d + 2
    p = pbinom(0.5*N + d, N, 0.5)
    
  }
  
  return (d)
  
}

ds = sapply(N, votes)

       