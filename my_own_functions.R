cv <- function(x) {
  coeff <- sd(x)/mean(x)
  return(coeff)
}

library(vegan)
even <- function(x) {
  div <- diversity(x, index = "shannon")
  species <- specnumber(x)
  evenness <- div/log(species)
  return(evenness)
}
