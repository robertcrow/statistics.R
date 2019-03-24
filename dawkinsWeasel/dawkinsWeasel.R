target = "METHINKS IT IS LIKE A WEASEL";
species = makeRandomString(1,28);
cur_score = 0;
generation = 1;

while ( ! (cur_score == 1)){
  
  output = generation(species, target, mutate, score, 100);
  cur_score = output$bestScore;
  species = output$newBest;
  generation = generation + 1;
  print(species);
  
}



