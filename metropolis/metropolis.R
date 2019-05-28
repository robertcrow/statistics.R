# travelling salesman problem 
#mydata = read.csv("pol.cv")
#plot.new()
head(pol)
coords.mx = matrix(rep(0, 2 * 25), 2, 25)
coords.mx[1,] = pol$x
coords.mx[2,] = pol$y

plot(c(coords.mx[1,], coords.mx[1,1]), c(coords.mx[2,], coords.mx[2,1]), col="blue")
#lines(c(coords.mx[1,], coords.mx[1,1]), c(coords.mx[2,], coords.mx[2,1]), col="blue")
threshold = 50
P = 0
T = 10
n_update = 0

for (i in 1:100000) {
  
  dists = sqrt(diff(coords.mx[1,])^2 + diff(coords.mx[1,])^2)
  cost = sum(dists)
  
  # switch order of two elements in coords.mx
  sample_ind = sample(pol$X1, 2)
  inv_sample_ind = c(sample_ind[2], sample_ind[1])
  
  tmp = coords.mx
  tmp[,sample_ind] = coords.mx[, inv_sample_ind]
  dists_new = sqrt(diff(tmp[1,])^2 + diff(tmp[1,])^2)
  cost_new = sum(dists_new)
  
  # delta = (cost - cost_new) / cost
  beta = (cost - cost_new)
  
  
  if (beta > 0) {
    n_update = n_update + 1
    print(c("update no:", n_update, "old cost:", cost, "new cost", cost_new))
    cost = cost_new
    coords.mx = tmp
    
  } else {
    
    P = exp(beta / T)
    criterium = runif(1) < P
    
    if (criterium) {
      
      n_update = n_update + 1
      print(c("update no:", n_update, "old cost:", cost, "new cost", cost_new))
      cost = cost_new
      coords.mx = tmp
      
    }
    
  } 
  
  T = T * 0.9
}


lines(c(coords.mx[1,], coords.mx[1,1]), c(coords.mx[2,], coords.mx[2,1]), col="red")




#tras<- sample(tras); with(pol[c(tras, tras[1]),], sum(sqrt(diff(x)^2 + diff(y)^2))) -> lens
#replicate(10000, tras<- sample(tras); with(pol[c(tras, tras[1]),], sum(sqrt(diff(x)^2 + diff(y)^2)))) -> lens
