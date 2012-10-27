#' Calculate the expected cluster memberships
#' @param ranks a list of ranking objects
#' @param thetas a matrix of weights on the metric, one for for each cluster
#' @param p the estimated proportions drawn from each cluster
#' @return the expected cluster memberships of each ranker
expect <- function(ranks, thetas = NULL, p) {
  z <- t(p*t(pRank(ranks, thetas)))
  z <- z/rowSums(z)# probabilities of each ranking under each model
  return(z)
}