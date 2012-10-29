# Internal functions

#' All information used to calculate Kendall's distance.
#' 
#' Performs each column-wise comparison on a matrix of sequences.  A 0 value
#' denotes that there is an increase between the two columns, 1 a decrease, and
#' NA indicates that the column values are identical in the row.
#' 
#' 
#' @param r Matrix of sequences.
#' @param inds Possibly efficiency increase when doing repeated calculations,
#' currently not used.
#' @return Matrix of 0s, 1s, and NAs representing pairwise comparisons of
#' vector values.
#' @author Erik Gregory
#' @references http://en.wikipedia.org/wiki/Kendall_tau_distance
#' @keywords Kendall Distance
kendallInfo <- function(r, inds = NULL) {
  if (is.null(inds)) {
    inds <- combn(ncol(r), 2)
  }
  if (!is.matrix(r)) {
    r <- as.matrix(r)
    attr(r, "dimnames") <- NULL
  }
  infos <- r[, inds[1, ]] - r[, inds[2, ]]
  decr <- which(infos > 0)
  incr <- which(infos < 0)
  indet <- which(infos == 0)
  infos[decr] <- TRUE
  infos[incr] <- FALSE
  infos[indet] <- NA
  return(infos)
}

#' Calculate the tie information for a set of rankings.
#' @param ranks a data frame of rankings, simplified so that there are no gaps in the numbers and the minimum rank is 1
#' @return A matrix where M[i, j] is the number of items in ranking 'i' which are ranked 'j'
tieInfo <- function(ranks) {
  top <- ncol(ranks) # Find the biggest rank observed
  tie.mat <- matrix(0, nrow = nrow(ranks), ncol = top)
  for(i in 1:top) {
    tie.mat[, i] <- apply(ranks, 1, function(j) length(which(j == i)))
  }
  tie.mat
}
relativeRank <- function(ranks) {
  N <- nrow(ranks)
  n <- ncol(ranks)
  temp <- ranks
  out <- matrix(0, nrow = N, ncol = n)
  for(i in 1:N) {
    n <- max(temp[i, ])
    for(j in 1:n) {
      ind <- which(temp[i, ] == j)
      out[i, ind] <- length(ind) - (1:length(ind)) + 1
    }
  }
  out
}

# N should be the number of items ranked
findV <- function(info, n) {
  temp <- info
  N <- nrow(info)
  V <- matrix(0, nrow = N, ncol = n - 1)
  for(j in 1:(n - 1)) {
    if (j < (n - 1)) {
      ind <- 1:(n - j)
      V[, j] <- rowSums(temp[, ind], na.rm = TRUE)
      temp <- temp[, -ind]
    }
    else {
      V[, j] <- temp
    }
  }
  V
}

geqInfo <- function(ranks, ties) {
  top <- ncol(ranks) # Greatest possible number of tie groups
  geq.mat <- matrix(0, nrow = nrow(ranks),
                    ncol = top)
  for(i in 1:top) {
    geq.mat[, i] <- apply(ties, 1, function(j) length(which(j >= i)))
  }
  geq.mat
}