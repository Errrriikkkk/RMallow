#' Update the theta parameters of a GMM for each cluster.
#' @param ranks A list of ranking objects, possibly of length 1.
#' @param z the ranker-wise cluster probabilities
#' @param min.pref the minimum theta weight which can be used
#' @return a matrix of the theta parameters of each cluster
Theta <- function(ranks, z, min.pref = 0.001) {
  G <- length(ranks)
  n <- ranks[[1]]$items
  theta <- lapply(1:G, function(i) {
    theta <- mclapply(1:(n - 1), function(j) {
      max.pref <- n - j + 1
      V <- sum(z[, i]*ranks[[i]]$V[, j], na.rm = TRUE)
      flower <- thetaObj(V, n, j, z[, i], ranks[[i]]$relative[, j], min.pref)
      fupper <- thetaObj(V, n, j, z[, i], ranks[[i]]$relative[, j], max.pref)
      if(sign(flower) != sign(fupper)) {
        theta <- uniroot(thetaObj, V = V,
                               n = n, j = j, 
                               z = z[, i], relative = ranks[[i]]$relative[, j],
                               interval = c(min.pref, max.pref), 
                               f.lower = flower,
                               f.upper = fupper)$root
      }
      else {
        #cat(paste("fupper:", fupper, ", flower:", flower, ", Cluster:", i, ", Theta:", j, '\n'))
        theta <- min.pref
      }
      theta
    })
    unlist(theta)
  })
  return(do.call('rbind', theta))
}
