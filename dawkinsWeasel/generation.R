generation <- function(curBest, target, FUN1 = mutate, FUN2 = score, population){

  curBest = rep(curBest, population);
  mutations = mutate(curBest);
  cur_score = score(mutations, target);
  bestScore = max(cur_score);
  newBest = mutations[cur_score == bestScore]
  
  return(newBest)
  
}