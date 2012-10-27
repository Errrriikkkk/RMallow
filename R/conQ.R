#' Construct the weighted transitions matrix.
#' @param ranks an object of class 'ranking'
#' @param pvals a vector of probabilities to weight the transitions
#' with for each individual.
#' @return a list of transitions matrices, one for each input in the list \code{ranks}
conQ <- function(ranks, pvals = 1) {
  abils <- items(ranks)
  temp1 <- info(ranks)
  namez <- names(ranks(ranks))
  temp <- colMeans(abs(temp1 - 1)*pvals, 
                   na.rm = TRUE) # Number of abilities
  temp2 <- colMeans(temp1*pvals, 
                    na.rm = TRUE)
  temp <- temp/(temp + temp2)
  Qmat <- matrix(0, nrow = abils, ncol = abils) # Q matrix, where the diagonal is 0
  for (i in 1:(abils - 1)) {
    Qmat[i, (i + 1):abils] <- temp[1:(abils - i)]
    Qmat[(i + 1):abils, i] <- 1 - temp[1:(abils - i)]
    temp <- temp[-(1:(abils - i))]
  }
  dimnames(Qmat) <- list(namez, namez)
  #lowerTriangle(Qmat) <- 1 - upperTriangle(Qmat)
  return(Qmat)
} 
