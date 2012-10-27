# TODO: Eliminate redundancies, possibly implement SearchPi in C
#       Consider allowing greedy search after a certain threshold when running SearchPi
#' Initialize sequence modes for the clustering process.
#'
#' @param G number of cluster centers, including the hypothesis if provided
#' @param hyp a single sequence of length \code{abils} to initialize one of the cluster centers
#' @param abils number of items being ranked
#' @return A list of G cluster centers, each of length abils
#' @author Erik Gregory
#' @examples
#' #rGen(3, 1:5, 5)
rGen <- function(G, hyp = NULL, abils) {
  R <- list()
  n <- 1
  if (!is.null(hyp)) {
    R <- hyp
    n <- length(R) + 1
  }
  if(n < G + 1) {
    for(i in n:G) {
      R[[i]] <- sample(abils)
    }
  }
  if (length(R) > 1) {
    R <- do.call('rbind', R)
  }
  else {
    R <- matrix(unlist(R), nrow = 1)
  }
  return(R)
}
