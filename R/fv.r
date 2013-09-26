#' Function to run the Fligner-Verducci heuristic on a set of rankings,
#' as extended to potentially partial rankings on mixtures of 
#' generalized Mallows models.
#' @param ranks an object of class ranking
#' @param z vector where v[i] represents the probability that ranking
#' @param type either "generalized" or not, used to determine the type of theta to search for
#' @param top.bound the maximum value of the theta parameter in a classic mallows model
#' @param local should the local search around the best guess for the mode be performed?
#' @return the hypothesized modal sequence, theta parameters, and likelihood found.
fv <- function(ranks, z, type = "generalized", top.bound = 1000, local = FALSE) {
  n <- ranks$items
  Qs <- conQ(ranks, z) # Determine the Q matrix for this cluster
  sig0 <- order(colSums(Qs)) # initial guess at mode for the cluster
  ranks1 <- ranking$new(ranks$ranks[, sig0]) # change ranking ordering
  if (type == "generalized") {
    thetas <- Theta(list(ranks1), matrix(z, ncol = 1)) # estimate thetas
  }
  else {
    thetas <- findLambda(ranks1, top.bound, matrix(z, ncol = 1))
  }
  params <- thetas
  best <- clusterLikelihood(list(ranks1), thetas, z) # initial log-likelihood
  found <- FALSE
  while(!found & local) {
    change <- FALSE
    cands <- neighbors(sig0) # neighbors of sig0
    for(i in 1:(n - 1)) {
      ranks1 <- ranking$new(ranks$ranks[, cands[i, ]]) # try it as the modal sequence
      if (type == "generalized") {
        theta <- Theta(list(ranks1), matrix(z, ncol = 1))
      }
      else {
        theta <- findLambda(ranks1, top.bound, matrix(z, ncol = 1))
      }
      like <- clusterLikelihood(list(ranks1), theta, z)
      if (like > best) {
        sig0 <- cands[i, ]
        best <- like
        params <- theta
        change <- TRUE
        print(sig0)
      }
    }
    if(!change) {
      found <- TRUE
    }
  }
  return(list(sig0, params, best))
}