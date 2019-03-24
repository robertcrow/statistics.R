makeRandomString <- function(n_strings, char_length)
{
  randomString <- c(1:n_strings)                  # initialize vector
  for (i in 1:n_strings)
  {
    randomString[i] <- paste(sample(c(LETTERS),
                      char_length, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}
