#' The objective function to find the zero of to find the lambda parameter of the model.
#' 
#' @param x The current guess at lambda
#' @param N Number of subjects probably in the cluster
#' @param D The observed distance of the subjects in the cluster
#' @param geqties Tie group information for the individuals assigned to the cluster.
#' @param abils The number of items ranked in the data.
#' @return The value of the objective function with the input parameters.  Goal is zero.
#' 
lambdaRoot <- function(x, N, D, geqties, abils) {
  inds <- 1:abils
  y <- sum((geqties - N)*(inds*x^inds)/(1 - x^inds)) - D
  return(y)
}
