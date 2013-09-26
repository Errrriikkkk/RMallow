#' Update the theta parameters of a GMM for each cluster.
#' @param ranks A list of ranking objects, possibly of length 1.
#' @param z the ranker-wise cluster probabilities
#' @param min.pref the minimum theta weight which can be used
#' @return a matrix of the theta parameters of each cluster
Theta <- function(ranks, z, min.pref = 0.001) {
  G <- length(ranks)
  n <- ranks[[1]]$items
  theta <- matrix(0, nrow = G, ncol = (n - 1))
  for(i in 1:G) {
    for(j in 1:(n - 1)) {
      max.pref <- n - j + 1
      V <- sum(z[, i]*ranks[[i]]$V[, j], na.rm = TRUE)
      flower <- thetaObj(V, n, j, z[, i], ranks[[i]]$relative[, j], min.pref)
      fupper <- thetaObj(V, n, j, z[, i], ranks[[i]]$relative[, j], max.pref)
      if(sign(flower) != sign(fupper)) {
        theta[i, j] <- uniroot(thetaObj, V = V,
                               n = n, j = j, 
                               z = z[, i], relative = ranks[[i]]$relative[, j],
                               interval = c(min.pref, max.pref), 
                               f.lower = flower,
                               f.upper = fupper)$root
      }
      else {
        cat(paste("fupper:", fupper, ", flower:", flower, ", Cluster:", i, ", Theta:", j, '\n'))
        theta[i, j] <- min.pref
      }
    }
  }
  return(theta)
}