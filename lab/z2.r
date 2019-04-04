load('seriale.RData')
# median(runif(100))
# quantile(runif(10), c(.5))
# quantile(runif(10), c(.1, .5, .75))


# head(names(seriale))
# which.max(sapply(seriale, length))
# max(sapply(seriale, length))

# which.max(sapply(seriale, median))
# which.min(sapply(seriale, median))

# sapply(seriale, function(x) diff(quantile(x, c(.25, .75))))



# korelacja spearmana
# korelacja pearsona

len <- sapply(seriale, function(x) length(x))
min_val <- sapply(seriale, function(x) min(x))
max_val <- sapply(seriale, function(x) max(x))
q_1 <- sapply(seriale, function(x) quantile(x, .75))
q_3 <- sapply(seriale, function(x) quantile(x, .25))
series_parameter <- data.frame(len, q_1, q_3)
series_parameter$irq <- series_parameter$q_3 - series_parameter$q_1

cor(series_parameter$len, series_parameter$q_3, m="s")
cor(series_parameter$len, series_parameter$q_1, m="s")
cor(series_parameter$q_1, series_parameter$q_3, m="s")
cor(sample(series_parameter$q_1), series_parameter$q_3, m="s")
# cor(series_parameter, m="s")

series_parameter$med_ocena <- sapply(seriale, median)
series_parameter$mean_ocena <- sapply(seriale, mean)

# cor(series_parameter, m="s")

spear_test <- function(x, y){
	s <- cor(x, y, m="s")
	message(s)
	repl <- replicate(10000, cor(sample(x), y, m="s"))
	message(repl[1:10])
	message(sum(repl > abs(s)))
	return(repl[1:30])
	# return(sum(repl > s)/length(repl))
}

a <- spear_test(series_parameter$q_1, series_parameter$q_3)
print(a)

# to podobno jest wszystko w pspearman
