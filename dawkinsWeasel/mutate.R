mutate <- function(input_strings) {


  n_strings = length(input_strings)

  output = vector(mode = "character", length = n_strings)

  stringMutation <- function(input_strings) {

    current_char = input_strings


    if (length(current_char) == 1) {
      current_char = strsplit(current_char, split = "")
      current_char = unlist(current_char)
    }

    nchar = length(current_char)
    criteria = runif(nchar, min = 0, max = 100)
    chars_for_mutation = criteria <= 5.0
    n_chars_for_mutation = sum(chars_for_mutation)

    current_char[chars_for_mutation] = sample(c(LETTERS, " "), n_chars_for_mutation, replace =
                                                TRUE)

    output_char = paste((current_char), collapse = "")

  }

  output = lapply(input_strings, stringMutation)
  output = unlist(output)

  return(output)


}
