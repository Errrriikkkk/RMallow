#' Update the Lambda parameters of each cluster of a Mallows Model.
#'
#' @param ranks An object of class "ranking", or a list of such objects.
#' @param top.bound If necessary, what is the top bound for the lambda parameters?
#' @param z The current probabilities of cluster membership.
#' @return The updated lambda parameters, as a matrix.
findLambda <- function(ranks, top.bound = 1000, z) {
  if(!is.list(ranks)) {
    ranks <- list(ranks)
  }
  N <- ranks[[1]]$rankers
  abils <- ranks[[1]]$items
  G <- length(ranks)
  lambda <- 0*(1:G)
  for(i in 1:G) {
    N <- sum(z[, i]) # The weighted number of individuals in the cluster
    D <- sum(z[, i]*rowSums(ranks[[i]]$V), na.rm = TRUE)
    geqtie <- colSums(z[, i]*ranks[[i]]$geqties)
    rt <- uniroot(lambdaRoot, interval = c(1e-10, 0.9999999999), N = N, 
                  D = D, geqties = geqtie, 
                  abils = abils, tol = 1e-10)$root
    lambda[i] <- -log(rt)
  }
  lambda <- lapply(lambda, function(i) rep(i, abils - 1))
  lambda <- matrix(do.call('rbind', lambda), nrow = G)
  lambda
}
