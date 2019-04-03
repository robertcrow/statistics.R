score <- function(a, b){

  #stopifnot(length(a) == length(b))
  stopifnot(is.character(b));
  n_strings = length(a);

  coverage = rep(0, n_strings);

  compareStrings <- function(arg1, arg2) {

    tmp1 = strsplit(arg1, split="")
    tmp2 = strsplit(arg2, split="")

    tmp1 = unlist(tmp1)
    tmp2 = unlist(tmp2)

    nchar = length(tmp1)

    return (sum(tmp1 == tmp2) / nchar)
  }

  coverage = lapply(a, FUN=compareStrings, arg2=b)
  coverage = unlist(coverage)


  return(coverage)

}

