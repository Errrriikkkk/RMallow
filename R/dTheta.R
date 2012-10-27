#' Calculate Dtheta from a set of rankings to each theoretical modal 
#' sequence of the model. 
#' @param ranks a list of objects of class 'ranking', or 
#' something that can be converted to one.
#' @param thetas the spread parameters of the models, in a matrix
#' @return A matrix of the theta distances from each sequence to each mode
dTheta <- function(ranks, thetas = NULL) {
  if(!is.list(ranks)) {
    ranks <- list(ranks)
  }
  G <- length(ranks)
  N <- rankers(ranks[[1]])
  n <- items(ranks[[1]])
  out <- matrix(0, nrow = N, ncol = G)
  for(i in 1:G) {
    if(!is.null(thetas)) {
      out[, i] <- colSums(thetas[i, ]*t(V(ranks[[i]])), 
                          na.rm = TRUE)
    }
    else {
      out[, i] <- rowSums(V(ranks[[i]]), na.rm = TRUE)
    }
  }
  return(out)
}