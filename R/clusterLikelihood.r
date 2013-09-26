#' Find the likelihood of a single cluster of a Mallows mixture model
#' @param ranks an object of class "ranking"
#' @param thetas the theta matrix for the model
#' @param z the latent variables, where z[i] is the probability that ranking i is a member of this cluster
#' @return the sum of the probabilities that these rankings belong to this model
clusterLikelihood <- function(ranks, thetas, z) {
  ps <- pRank(ranks, thetas) # calculate the probabilities of the rankings
  ps <- z * ps # Multiply those p-values by the probability of cluster membership
  sum(ps) # Sum of those values is the likelihood of the data under a single cluster of gmm
}