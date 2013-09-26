#' Function to grab all permutations which are generated
#' by a transposition of adjacent items
#' @param sig a vector ranking to find the neighbors of
#' @return a matrix in which the rows are the neighbors of sig
#' @examples neighbors(c(1, 2, 3, 4))
neighbors <- function(sig) {
  n <- length(sig)
  inds <- cbind(1:(n - 1), 2:n)
  neigh <- matrix(0, nrow = (n - 1), ncol = n)
  for(i in 1:(n - 1)) {
    temp <- sig
    temp[inds[i, ]] <- temp[rev(inds[i, ])]
    neigh[i, ] <- temp
  }
  return(neigh)
}