mutate <- function(arg1){
  
  n_strings = length(arg1);
  output = vector(mode="character", length=n_strings);
  cursor = 1;
  index = 0;
 
  
  # spróbowac z tapply
  
  for (i in 1:n_strings) {
    
    current_char = arg1[i];
    print("current char is");
    print(current_char);
    print("mutating in progress....");
    if (length(current_char) == 1)
      current_char = strsplit(current_char, split="")
      current_char = unlist(current_char);
    nchar = length(current_char);
    
    coverage = rep(FALSE, nchar);
    print("number of characters to mutate:")
    print(nchar)
    print("number of mutated characters:")
    print("current char is");
    print(current_char);
      
    while (sum(coverage) < nchar) {
      #print(sum(coverage))
      cursor = cursor + 21;
      index = cursor %% nchar + 1;
      current_char[index] = sample(c(LETTERS, " "), 1, replace=TRUE);
      coverage[index] = TRUE;
      
      
    }
    print(current_char);
    output_char = paste((current_char),collapse="")
    print(output_char);
    output[i] = output_char;
    
    
  }
  
  return(output);
  
}

