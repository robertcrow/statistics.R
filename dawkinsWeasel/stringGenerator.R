nRandomStrings <- function(n_strings, char_length)
{
  randomString <- c(1:n_strings) # initialize vector
  genString <- function(char_len) paste(sample(c(LETTERS),char_len, replace=TRUE),collapse="")

  if (n_strings == 1) {
    randomString <- genString(char_length)

  } else if (n_strings > 1) {
    dummy = rep(28, n_strings)
    randomString = lapply(dummy, genString)

  }

  return(randomString)
}
