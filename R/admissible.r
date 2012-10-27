#' Calculate the admissible heuristic for the SearchPi Algorithm.
#' @param thetas a vector of theta values
#' @param V a matrix where V[i, j] represents the number of items in [j:n] ranked less than or equal to j by ranker i
#' @param j the index of the theta value in question
#' @param n the number of theta values, minus 1
#' @param Qmax the maximum value in the Q matrix
#' @return real number representing the lower bound on the cost-to-go
admissible <- function(thetas, V, j, n, Qmax) {
  Vmin <- min(V)
  vec <- (j + 1):(n - 1)
  tmp <- Vmin - (vec - j)*Qmax
  tmp[tmp < 0] <- 0
  A <- sum(thetas[vec]*tmp) # Admissible Heuristic
  return(A)
}