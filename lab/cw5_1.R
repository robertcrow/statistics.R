a = runif(10)
b = runif(10)

hist(c(a,b+20))
boxplot(list(a,b+20))
boxplot(list(a,b))

wilcox.test(a,b+20) $p.value

replicate(100, wilcox.test(runif(100), runif(100))$p.value)
replicate(1000, min(replicate(10, wilcox.test(runif(20), runif(20))$p.value)))->pvdd
hist(pvdd)                           

replicate(1000, min(p.adjust(replicate(10, wilcox.test(runif(20), runif(20))$p.value))))->pvddc
hist(pvddc)
                            
replicate(1000, wilcox.test(runif(100), runif(100))$p.value)->pv
c(rep(1e-5,999), pv)->pv
mean(p.adjust(pv, "fdr") < 0.05)
sum(p.adjust(pv, "fdr")<0.05)
length(pv)
sum(pv < 0.05)
