dawkinsWeasel <- function(nGen=0, target, input="", FUN1 = mutate, FUN2 = score) {

  logfile <- file("log", open = "wt");
  # sink(logfile)
  # sink("log.txt", type = "message")
  # sink('log.txt')
  # sink(file=logfile, type="message")
  target_length = nchar(target);

  if ( is.null(input) || !(nchar(input) == target_length)) {
    species = nRandomStrings(1,target_length);
    message("Input string not specified or of incompatible length...input replaced with random string");
    # cat(sprintf("Input string not specified or of incompatible length...input replaced with random string \n"))

    writeLines("Input string not specified or of incompatible length...input replaced with random string \n", logfile);
  }
  else {
    species = input;
  }

  cur_score = score(species, target);
  iter = 1;
  message("Generation:\t", iter, "\t Current score:\t", cur_score, "\t Species:\t", species);
  writeLines(c("Generation:", iter, "Current score:", cur_score, "Species:", species,"\n"), logfile);

  if (nGen == 0) {

    while ( ! (cur_score == 1)){

      output = generation(species, target, mutate, score, 100);
      cur_score = output$bestScore;
      species = output$newBest;
      iter = iter + 1;
      message("Generation:\t", iter, "\t Current score:\t", cur_score, "\t Species:\t", species);
      cat(c("Generation:\t", iter, "\t Current score:\t", cur_score, "\t Species:\t", species, "\n"), logfile);

    }

  }
  else {

    for (i in 1:(nGen-1)) {

      output = generation(species, target, mutate, score, 100);
      cur_score = output$bestScore;
      species = output$newBest;
      message("Generation:\t", i+1, "\t Current score:\t", cur_score, "\t Species:\t", species);
      cat(c("Generation:\t", i+1, "\t Current score:\t", cur_score, "\t Species:\t", species), "log");

    }

  }

  # sink()
  close(logfile)

}




