# travelling salesman problem 
#mydata = read.csv("pol.cv")

coords.x = pol$x
coords.y = pol$y

# inds = pol$X1
# inds = matrix(rep(inds[-1],Np),length(inds)-1, Np)

Np = 10
shuffle_time = 10000

coords.mx = matrix(rep(t(c(coords.x[-4], coords.x[1])), Np), 25, Np)
coords.my = matrix(rep(t(c(coords.y[-4], coords.y[1])), Np), 25, Np)

# initialization of supplementary variables for each column
P = rep(0, Np) #probability
T = 1 / 10^(seq(-2,4,6/Np)) # annealing temperature
T = T[1:Np] # seq work-around

# compute initial cost for each column

tmp.x = apply(coords.mx, 2, function(x) {diff(x)^2})
tmp.y = apply(coords.my, 2, function(x) {diff(x)^2})
tmp.sum = tmp.x + tmp.y
dists = apply(tmp.sum, 2, sqrt)
cost = apply(dists, 2, sum)

for (i in 1:100000) {
  
  # find new config
  
  inv.xy = apply(rbind(coords.mx, coords.my), 2, switch_inds)
  inv.x = inv.xy[1:25,] 
  inv.y = inv.xy[26:50,]
  
  new_tmp.x = apply(inv.x, 2, function(x) {diff(x)^2})
  new_tmp.y = apply(inv.y, 2, function(x) {diff(x)^2})
  
  new_tmp.sum = new_tmp.x + new_tmp.y
  new_dists = apply(new_tmp.sum, 2, sqrt)
  new_cost = apply(new_dists, 2, sum)

  # update solution if new is better
  
  beta = (cost - new_cost)
  arg1 = beta > 0
  cost[arg1] = new_cost[arg1]
  coords.mx[, arg1] = inv.x[, arg1]
  coords.my[, arg1] = inv.y[, arg1]
  
  # in case of a worse solution -> assign value depending on probability
  P[!arg1] = exp(beta[!arg1] / T[!arg1])
  criterium = runif(Np) < P
  arg2 = criterium & !arg1
  
  cost[arg2] = new_cost[arg2]
  coords.mx[, arg2] = inv.x[, arg2]
  coords.my[, arg2] = inv.y[, arg2]
  
  if (i%% shuffle_time){    # every n iters shuffle solutions between temperatures

    coords = shuffle_solutions(rbind(coords.mx, coords.my))
    coords.mx = coords[1:25,]
    coords.my = coords[26:50,]

  }

}

#choose best solution

ind_best_solution = which.min(cost)
best_solution = cost[ind_best_solution]

# plot solution
plot(coords.mx[, ind_best_solution], coords.my[, ind_best_solution], col="blue")
lines(coords.mx[, ind_best_solution], coords.my[, ind_best_solution], col="red")





############################################
# SUPPLEMENTARY FUNCTIONS

# function for switching two inds in a set

switch_inds <-function(x){
    
  len = length(x) / 2
  inds = sample(2:(len - 1), 2)
  inv_inds = c(inds[2], inds[1])
  x[inv_inds] = x[inds]
  x[inv_inds+25] = x[inds+25]
  return (x)
}

# shuffle function for swaping solutions between different temperature levels

shuffle_solutions <- function(x){
  
  x_dim = dim(x)
  len = x_dim[2]
  inds = 1:len
  shuffle_inds = sample(inds, len)
  inv_inds = c(inds[2], inds[1])
  
  x[,inds] = x[, inv_inds]
  return (x)
  
}
