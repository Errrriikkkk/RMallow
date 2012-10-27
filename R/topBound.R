#' Calculate the upper bound on the cost to go for the Branch-and-Bound algorithm.
#' @param thetas see \code{thetas}
#' @param V see \code{V}
#' @param j the current theta index
#' @param n the number of items ranked
#' @param Qmin the minimum value of the Q matrix
#' @return the top bound on the cost to go
topBound <- function(thetas, V, j, n, Qmin) {
  Vmax <- max(V)
  vec <- (j + 1):(n - 1)
  tmp1 <- Vmax - (vec - j)*Qmin
  tmp1[tmp1 < 0] <- 0
  top1 <- sum(thetas[vec]*tmp1) # Top bound for B&B algorithm
  return(top1)
}
