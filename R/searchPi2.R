#' Perform a search for a fixed number of iterations for a modal sequence.
#' @param queue is the search frontier
#' @param L is the lower-bound on the total cost for the sequences in \code{queue}
#' @param C is the incurred cost of each sequence in queue.
#' @param thetas is a vector of weights
#' @param Qs is the transitions matrix of the data
#' @param iter is the number of seqs to test before returning to the main loop with results.
#' @param pos is the last non-null position in the queue plus 1
#' @param best the best sequence found so far
#' @param LUB the least upper bound on the final cost
#' @return an updated search object
searchPi2 <- function(Qs, thetas, L, C,  
                      iter = 1000, pos, queue, best = NULL,
                      LUB = 100000) {
  n <- nrow(Qs) # Number of items ranked
  ind <- 1:n
  l <- 0
  closed <- FALSE
  last.best <- 0
  # Until a break is encountered, continue searching
  while(TRUE) {
    l <- l + 1 # Count iterations
    a <- which.min(L)[1] # Find best element in frontier
    best <- queue[[a]] # Remove the best element from the frontier
    j <- length(best)
    Ctemp <- C[a] # Incurred cost of best element
    if(j == n) { # Break if we have found the best sequence
      closed <- TRUE
      best.cost <- C[a]
      if (C[a] < LUB) {
        LUB <- C[a]
      }
      queues <- list()
      L <- 0*(1:2)
      C <- 0*(1:2)
      if(length(best) < 2) {
        best <- 1:2
      }
      break
    }
    if (L[a] >= LUB) { # Break if the lower bound on the cost is too great
      print(L[a])
      print(best)
      print(LUB)
      closed <- TRUE
      best.cost <- C[a]
      queues <- list()
      L <- 0*(1:2)
      C <- 0*(1:2)
      if(length(best) < 2) {
        best <- 1:2
      }
      break
    }
    else {
      to.use <- ind[-best] # items to join with best
      n1 <- length(to.use)
      p <- list() # expanded nodes
      V <- 0*(1:n1)
      Q.temp <- Qs[-best, ]
      Qmin <- min(Q.temp[Q.temp != 0])
      Qmax <- max(Q.temp)
      for(i in 1:n1) { # Create the children of the best node.
        p[[i]] <- c(best, to.use[i])
        V[i] <- sum(Qs[-best, to.use[i]]) # Calculate V for children
      }
      # V <- colSums(Qs[-best, to.use]) # might be the vectorized implementation
      A <- 0
      top1 <- 10000
      if(j < n - 1) {
        A <- admissible(thetas, V, j, n, Qmax)
        top1 <- topBound(thetas, V, j, n, Qmin)
      }
      for(i in 1:n1) { # add children to frontier, if applicable
        cost <- Ctemp
        if(j < n - 1) {
          cost <- Ctemp + thetas[j + 1]*V[i]
        }
        like <- cost + A # Lower bound on final cost for the node
        if (like <= LUB) { # Add to the queue, if it has potential to be the best
          queue[[pos]] <- p[[i]]
          L[pos] <- like
          C[pos] <- cost
          pos <- pos + 1
        }
        worst.case <- cost + top1 # check the LUB implied by the node
        if (worst.case < LUB) { # Update LUB for cluster, all clusters if necessary
          LUB <- worst.case
        }
      }
      queue <- queue[-a]
      L <- L[-a]
      C <- C[-a]
      pos <- pos - 1
    }
    if (l == iter) {
      best <- best
      best.cost <- C[a]
      break
    }
  }
  return(list(best = best, C = C, L = L, 
              queue = queue, pos = pos, closed = closed,
              LUB = LUB, best.cost = best.cost)) #return best sequence, all costs, all lower bounds,
  # the frontier, and the LUB on the cost.
}