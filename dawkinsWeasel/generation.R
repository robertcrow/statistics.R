generation <- function(curBest, target, FUN1 = mutate, FUN2 = score, population){

  curBest = rep(curBest, population);
  mutations = mutate(curBest);
  cur_score = score(mutations, target);
  bestScore = max(cur_score);
  bestFits = mutations[cur_score == bestScore];
  
  newBest = bestFits[sample(1:length(bestFits),1)]
  output <- list("newBest" = newBest, "bestScore" = bestScore)
  
  return(output)
  
}