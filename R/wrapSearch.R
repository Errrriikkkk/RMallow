#' A wrapper for the parallel function \code{searchPi2}
#' @param Qs list of Q matrices
#' @param thetas matrix of theta values
#' @param inp the other information used for the search
#' @param closed a vector representing which clusters are open and closed
#' @param iter the number of iterations before returning to the parent cluster
#' @param LUB a vector of the Least Upper Bounds on the cost-to-go for each cluster
#' @return an updated search object
wrapSearch <- function(Qs, thetas, inp, closed, iter = 1000, LUB) {
  # Only run searchPi if there is hope of finding a better answer
  G <- length(inp)
  for(i in 1:G) {
    if(closed[i] == 0) {
      inp[[i]] <- searchPi2(Qs = Qs[[i]], thetas = thetas[i,], 
                            L = inp[[i]][["L"]], C = inp[[i]][["C"]], 
                            LUB = inp[[i]][["LUB"]], iter = iter, 
                            pos = inp[[i]][["pos"]], queue = inp[[i]][["queue"]],
                            best = inp[[i]][["best"]])
      break
    }
  }
  return(inp)
}