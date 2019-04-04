load("~/ICM/statystykaR/imdbCorrelations/seriale.RData")
head(seriale)
max(seriale)

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
