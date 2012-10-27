#' Find the normalizing coefficient of a Generalized Mallows model
#' given the tie information of the data and the spread parameters
#' @param ranks A list of objects of class 'ranking'
#' @param thetas Matrix of spread parameters, one row for each object in the list of ranks
#' @return A matrix of the normalizing coefficients for the theta values
# For each sequence we return the normalizing coefficients under each model, which are
# \prod_{j = 1}^{n-1}\left[\frac{1 - \exp[-k_j(y\circ\pi^{-1})\theta_j]}
#                               {1 - \exp[-(n - j + 1)\theta_j]}\right]
normalize <- function(ranks, thetas = NULL) {
  if(!is.list(ranks)) {
    ranks <- list(ranks)
  }
  n <- items(ranks[[1]])
  N <- rankers(ranks[[1]])
  G <- length(ranks)
  out <- matrix(0, nrow = N, ncol = G)
  for(i in 1:G) {
    numers <- 1 - exp(t(-thetas[i, ]*t(relative(ranks[[i]])[, 1:(n - 1)])))
    denom <- (1 - exp(-(n - 1:(n - 1) + 1)*thetas[i, ]))
    temp <- numers/denom
    out[, i] <- apply(temp, 1, prod)
  }
  return(out)
}