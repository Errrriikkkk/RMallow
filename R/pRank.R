#' Probabilities of a set of rankings given some modes andspread parameters
#'
#' @param ranks a list of objects of class 'ranking',
#' one for each mode.
#' @param thetas a matrix of spread parameters for each cluster
#' @return A matrix where the out[i, j] is the probabilty of 
#' ranking 'i' under model 'j'.
pRank <- function(ranks, thetas) {
  norm <- normalize(ranks, thetas) # Normalizing coefficients
  dthetas <- dTheta(ranks, thetas)# Distances from the modes, as a function of theta
  out <- norm*exp(-dthetas)
  return(out)
}
