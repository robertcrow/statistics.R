score <- function(a, b){
  
  stopifnot(length(a) == length(b))
  stopifnot(assertthat::is.string(a));
  stopifnot(assertthat::is.string(b));
  
  tmp1 = strsplit(a, split="");
  tmp2 = strsplit(b, split="");
  
  
  tmp1 = unlist(tmp1);
  tmp2 = unlist(tmp2);
  
  nmatches<-sum(tmp1==tmp2);
  nchar = length(tmp1);
  
  
  coverage = nmatches / nchar;
  
  return(coverage)
  
}

