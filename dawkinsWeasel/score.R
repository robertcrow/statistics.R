score <- function(a, b){
  
  #stopifnot(length(a) == length(b))
  stopifnot(is.character(a));
  stopifnot(assertthat::is.string(b));
  n_strings = length(a);
  coverage = rep(0, n_strings);
  
  
  for (i in 1:n_strings){
    
    a_tmp = a[i];
    
    tmp1 = strsplit(a_tmp, split="");
    tmp2 = strsplit(b, split="");
    
    tmp1 = unlist(tmp1);
    tmp2 = unlist(tmp2);
    
    nmatches<-sum(tmp1==tmp2);
    nchar = length(tmp1);
    
    coverage[i] = nmatches / nchar;
    
  }

  return(coverage)
  
}

