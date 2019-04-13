#load("~/ICM/statystykaR/imdbCorrelations/seriale.RData")
head(seriale)

# series wth max num of ratings
ex1_val = which.max(unlist(lapply(seriale, FUN=length)))
ex1_name = names(seriale[ex1_val])

# series with max median
ex2_val = which.max(unlist(lapply(seriale, FUN=median)))
ex2_name = names(seriale[ex2_val])

# series interquantile measure (half of range)
ex3_values = unlist(lapply(seriale, FUN=IQR)) / 2
ex3_name = names(seriale[which.max(ex3_values)])

# CORRELATION:
# 1) between series duration and ratings
mean_ratings = unlist(lapply(seriale, FUN=mean))
num_of_episodes = unlist(lapply(seriale, FUN=length))
cor1 = cor(num_of_episodes, mean_ratings, method="spearman")

# 2) between Q1/Q3 and range of ratings
tmp2_1 = unlist(lapply(seriale, FUN=IQR)) / 2
tmp2_2 = unlist(lapply(seriale, FUN=max)) - unlist(lapply(seriale, FUN=min)) 
cor2 = cor(tmp2_1, tmp2_2, method="spearman")

# PERMUTATION TEST

permutationTest <- function(xo, yo, n){
  
  cor_values = rep(NaN,n)
  cor_values[1] = cor(xo, yo, method="spearman")
  
  x = matrix(rep(xo, n-1), length(xo), n-1)
  y = matrix(rep(yo, n-1), length(yo), n-1)
  
  x = sapply(1:ncol(x), function(var1, var2){ var2[sample(1:nrow(x)),var1]}, x)
  y = sapply(1:ncol(y), function(var1, var2){ var2[sample(1:nrow(x)),var1]}, y) 
  
  cor_values[2:length(cor_values)] = sapply(1:ncol(x), function(var1, var2, var3)
    {
      cor(var2[,var1], var3[,var1], method="spearman")
    }, x, y)
  
  p_value = sum(cor_values[2:length(cor_values)] > cor_values[1]) / (n-1)
  
  return (p_value)
}

# permutationTest2 <- function(xo, entry, n){
#   
#   median_values = unlist(lapply(unlist(seriale), FUN=median)
#   cor_values[1] = cor(xo, yo, method="spearman")
#   
#   x = matrix(rep(xo, n-1), length(xo), n-1)
#   y = matrix(rep(yo, n-1), length(yo), n-1)
#   
#   x = sapply(1:ncol(x), function(var1, var2){ var2[sample(1:nrow(x)),var1]}, x)
#   y = sapply(1:ncol(y), function(var1, var2){ var2[sample(1:nrow(x)),var1]}, y) 
#   
#   cor_values[2:length(cor_values)] = sapply(1:ncol(x), function(var1, var2, var3)
#   {
#     cor(var2[,var1], var3[,var1], method="spearman")
#   }, x, y)
#   
#   p_value = sum(cor_values[2:length(cor_values)] > cor_values[1]) / (n-1)
#   
#   return (p_value)
# }

# P-VALUES OF CORRELATIONS

p_val1 = permutationTest(mean_ratings, num_of_episodes, 100)
p_val2 = permutationTest(tmp2_1, tmp2_2, 10000)

# P-VALUES for median



#ratings_median = unlist(lapply(seriale, FUN=median()))



