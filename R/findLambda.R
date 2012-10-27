#' Update the Lambda parameters of each cluster of a Mallows Model.
#'
#' @param ranks An object of class "ranking".
#' @param top.bound If necessary, what is the top bound for the lambda parameters?
#' @param G Number of clusters in the model.
#' @param N Number of rows in the rankings.
#' @param z The current probabilities of cluster membership.
#' @return The updated lambda parameters, as a vector.
findLambda <- function(ranks, top.bound, G, N, z) {
  # For each cluster
  if(!is.list(ranks)) {
    ranks <- list(ranks)
  }
  abils <- ncol(ranks(ranks[[1]]))
  lambda <- 0*(1:G)
  for(i in 1:G) {
    N <- sum(z[, i])
    D <- sum(z[, i]*rowSums(V(ranks[[i]]), na.rm = TRUE))
    geqtie <- colSums(z[, i]*geqties(ranks[[i]]))
    rt <- uniroot(lambdaRoot, interval = c(1e-10, 0.9999999999), N = N, 
                  D = D, geqties = geqtie, 
                  abils = abils, tol = 1e-10)$root
    lambda[i] <- -log(rt)
  }
  return(lambda)
}
