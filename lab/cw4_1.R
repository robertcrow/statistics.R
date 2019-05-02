# Monte Carlo methods

#Birthday paradox

monteCarloBday <- function(myBday, n){
  
  bdays = lapply(rep(n, 1000), function(n){sample(1:365, n, replace=TRUE)})
  score = lapply(1:length(bdays), function(i, bdays){sum(unlist(bdays[i]) == my_bday)}, bdays)
  score = mean(unlist(score) / n);
  
  ref_score = 1/365
  return(score / ref_score)
  
}


# n = 10
output10 = monteCarloBday(87, 10)

# n = 20
output20 = monteCarloBday(87, 20)

# n = 50
output50 = monteCarloBday(87, 50)

# n = 100
output100 = monteCarloBday(87, 100)



# Finding duplicates

bp <- function(yd, n) mean(replicate(1000, any(table(sample(1:yd, n, replace=TRUE))>1)))
sapply(c(10,20,50,100), function(n) bp(365, n))

# The Zygmunt Hajzer problem

generateBoxes <- function(n) {
    box = rep(0, n)
    box[sample(1:n, 1)] = 1
    return(box)
  }

# test1 - change box 
test1 <- function(first_choice) {
  
  hint = sample(1:3, 1, replace=TRUE)
  
  if (hint == first_choice) output = 0
  else {
    
    third_option = which(1:3 != hint & 1:3 != first_choice)
    ind = sample(1:2, 1, replace=TRUE)
    
    if (first_choice == third_option) {output = 0}
    else {output = 1}
    
  }
  
  return(output)
  }
  
  
scores = lapply(replicate(1000,sample(1:3, 1, replace=TRUE)), test1)
value1 = 1 - sum(unlist(scores))/1000

# test2 - stay with previou10000s choice 

test2 <- function(first_choice) {
  
  hint = sample(1:3, 1, replace=TRUE)
  
  if (hint == first_choice) output = 0
  else {
    
    third_option = which(1:3 != hint)
    ind = sample(1:2, 1, replace=TRUE)
    index = third_option[ind]
    if (first_choice == index ) {output = 1}
    else {output = 0}
    
  }
  
  return(output)
}

scores2 = lapply(replicate(10000,sample(1:3, 1, replace=TRUE)), test2)
value2 = 1 - sum(unlist(scores))/10000



box = generateBoxes(3)


