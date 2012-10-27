#' The objective function used to solve for \code{theta}
#' 
#' @param V see \code{V}
#' @param n the number of items ranked
#' @param j the current item we are finding the theta value for
#' @param z the expected cluster memberships of the data
#' @param relative see \code{relative}
#' @param theta the current guess of \code{theta}
#' @return the value of the objective function with the given parameters.  Goal is zero.
thetaObj <- function(V, n, j, z, relative, theta) {
  temp <- relative/(exp(relative*theta) - 1) - (n - j + 1)/(exp((n - j + 1)*theta) - 1)
  temp <- z*temp
  return(sum(temp) - V)
}