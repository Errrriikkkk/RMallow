#' Find the modal sequence using a \code{Q} matrix.
#' @param Qs a transitions matrix of a data set
#' @param thetas a matrix of weights on the metric
#' @return the best possible modal sequence for the cluster
searchPi <- function(Qs, thetas) {
  Qmax <- max(Qs)
  Qmin <- min(Qs[Qs != 0])
  n <- nrow(Qs) # number of items ranked
  ind <- 1:n
  best.worst <- 10000 # Lower bound on the cost-to-go
  L <- rep(1000, 100000)
  L[ind] <- colSums(Qs)*thetas[1]
  C <- L
  queue <- vector(mode = "list", 100000)
  queue[ind] <- as.vector(ind, mode = "list")
  iter <- 0
  pos <- n + 1
  # Until a break is encountered, continue searching
  while(TRUE) {
    iter <- iter + 1
    a <- which.min(L)[1]
    best <- queue[[a]] # Remove the best element from the frontier
    j <- length(best)
    Ctemp <- C[a]
    if(j == n) {
      break
    }
    else {
      to.use <- ind[-best] # items to join with best
      n1 <- length(to.use)
      p <- list() # expanded nodes
      V <- 0*(1:n1) # V values
      Q.temp <- Qs[-best, ]
      Qmin <- min(Qs[Qs != 0])
      Qmax <- max(Q.temp)
      for(i in 1:n1) { # we can probably vectorize...
        p[[i]] <- c(best, to.use[i])
        V[i] <- sum(Qs[-best, to.use[i]])
      }
      if(j < n - 1) {
        Vmin <- min(V)
        Vmax <- max(V)
        vec <- (j + 1):(n - 1)
        tmp <- Vmin - (vec - j)*Qmax
        tmp1 <- Vmax - (vec - j)*Qmin
        tmp[tmp < 0] <- 0
        tmp1[tmp1 < 0] <- 0
        A <- sum(thetas[vec]*tmp)
        top1 <- sum(thetas[vec]*tmp1)
      }
      for(i in 1:n1) {
        cost <- Ctemp
        if(j < n - 1) {
          cost <- cost + thetas[j + 1]*V[i]
        }
        like <- cost + A
        # Only add to the queue if the node has the potential to be the best node.
        #if (like < best.worst) {
        queue[[pos]] <- p[[i]]
        L[pos] <- like
        C[pos] <- cost
        pos <- pos + 1
        #}
        worst.case <- cost + top1
        if (worst.case < best.worst) {
          best.worst <- worst.case
          best.worst.seq <- p[[i]]
        }
      }
      queue <- queue[-a]
      L <- L[-a]
      C <- C[-a]
      pos <- pos - 1
    }
    if(iter %% 500 == 0) {
      print(paste("Best:", paste(best, collapse = " ")))
      print(paste("Iteration:", iter))
      print(paste("queue size:", pos))
      #       print(paste("Best.worst", best.worst))
      #       print(paste("Best.worst.seq", paste(best.worst.seq, collapse= " ")))
    }
  }
  #cat(paste("SearchPi converged in", iter, "iterations\n"))
  return(best)
}
