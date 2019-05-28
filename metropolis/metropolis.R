# travelling salesman problem 
#mydata = read.csv("pol.cv")

# simulation parameters
nc = 24
np = 10
shuffle_time = 1000

# initialization of coords lists

coords_x = pol$x
coords_y = pol$y
coords_x = rep(list(c(coords_x[-4], coords_x[1])), np)
coords_y = rep(list(c(coords_y[-4], coords_y[1])), np)

# initialization of supplementary variables for city sequence
P = rep(0, np) #probability
T = 1 / 10^(seq(-2, 4, length.out = np))

# compute initial cost for each column
cost = rep(10000, np)
tmp = rep(25, 10)

for (i in 1:100000) {
  
  # find new config
  
  inds = lapply(tmp, FUN=shuffle_inds)
  coords_x_new = mapply(shuffle_coords, coords_x, inds, SIMPLIFY = FALSE)
  coords_y_new = mapply(shuffle_coords, coords_y, inds, SIMPLIFY = FALSE)
  
  # find new cost
  tmp_x = lapply(coords_x_new, function(x) {sum(diff(x)^2)})
  tmp_y = lapply(coords_y_new, function(x) {sum(diff(x)^2)})
  new_tmp.sum = mapply(sum, tmp_x, tmp_y)
  
  
  dists_new = sapply(new_tmp.sum, sqrt)
  cost_new = sapply(dists_new, sum)

  # update solution if new is better
  # criterium no1: better score
  beta = (cost - cost_new)
  arg1 = beta > 0
  
  # in case of a worse score -> assign value depending on probability
  P = exp(beta / T)
  arg2 = runif(np) < P
  
  # update solutions
  
  inds_update = (arg1 | arg2)
  cost[inds_update] = cost_new[inds_update]
  
  coords_x[which(inds_update)] = coords_x_new[which(inds_update)]
  coords_y[which(inds_update)] = coords_y_new[which(inds_update)]
  
  # if 1 in n iteration -> shuffle temperatures
  
  if (i%% shuffle_time){T = shuffle_solutions(T)}


}

#choose best solution

ind_best_solution = which.min(cost)
best_solution = cost[ind_best_solution]

# plot solution
plot(coords_x[[ind_best_solution]], coords_y[[ind_best_solution]], col="blue")
lines(coords_x[[ind_best_solution]], coords_y[[ind_best_solution]], col="red")





############################################
# SUPPLEMENTARY FUNCTIONS

# function for switching two inds in a set

shuffle_inds <- function(x){
  
 inds = sample(2:(x-1), 2)
 return(inds)
  
}
  
  
shuffle_coords <-function(x,y){
    
  inv_y = c(y[2], y[1])
  x[inv_y] = x[y]
  return (x)
  
}

# shuffle function for swaping solutions between different temperature levels

shuffle_solutions <- function(x){
  
  len = length(x)
  inds = 1:len
  shuffle_inds = sample(inds, len)
  
  x = x[shuffle_inds]
  return (x)
  
}
