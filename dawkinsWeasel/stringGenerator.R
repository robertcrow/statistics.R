makeRandomString <- function(n, nchar)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(LETTERS),
                        nchar, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}