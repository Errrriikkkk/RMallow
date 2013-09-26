#' Perform a parallel search for the modes of a Mallows Mixture model on a data set.
#' @param Qs a list of transitions matricies, one for each cluster
#' @param thetas the matrix of weights on the metric for each cluster
#' @param cores the number of cores used to perform the search
#' @param best.worst the upper bound on the final cost
#' @param iter the number of iterations after which the searchers return to the parent to consolidate findings
#' @return a list of the modes found, one for each cluster
parallelSearch <- function(Qs, thetas, cores = 4, 
                           best.worst = 10000, iter = 500) {
  n <- nrow(Qs[[1]]) # Number of items ranked
  cores <- min(cores, round(n/2 - 0.5)) # We dont use multiple cores if there are too few items ranked
  count <- 1
  G <- nrow(thetas) # Number of clusters
  queues <- createInps(Qs, thetas, cores = cores) # Inputs for the search
  out <- list() # List of all modal sequences found
  LUB <- rep(best.worst, G) # Least upper bounds found for each cluster
  best.like <- rep(best.worst, G)
  closed <- matrix(0, nrow = G, ncol = cores)
  found <- FALSE
  if (suppressWarnings(require(multicore))) {
    fn <- mclapply
  }
  else {
    fn <- lapply
  }
  while(found == FALSE) { # iterate until all threads, nodes are done.
      queues <- fn(1:cores, function(k)
                       wrapSearch(Qs, thetas, queues[[k]],
                       closed[, k], iter = iter,
                       LUB = LUB))
    for (j in 1:G) {
      for (k in 1:cores) {
        LUB[j] <- min(queues[[k]][[j]]$LUB, LUB[j])
      }
    }
    cat(paste("\nParallel SearchPi* iteration", count, ":\n"))
    for(j in 1:G) { # Consolidate LUBs, closed threads and clusters
      for(i in 1:cores) { # For each thread
        if(closed[j, i] == 0) {
          Ls <- min(queues[[i]][[j]][["L"]])
          printInfo(Ls, queues[[i]][[j]][["pos"]], 
                    i, j, queues[[i]][[j]][["best"]], LUB[j])
          if(Ls > LUB[j] | queues[[i]][[j]][["closed"]] == TRUE) { # Close the node if it cannot be the best
            queues[[i]][[j]][["closed"]] <- TRUE
            closed[j, i] <- 1
          }
        }
        queues[[i]][[j]][["LUB"]] <- LUB[j] # Make sure the LUB is as low as possible
        if (length(queues[[i]][[j]][["best"]]) == n) { # If a full-length sequence
          if (queues[[i]][[j]][["best.cost"]] < best.like[j]) { # If best full-length sequence
            out[[j]] <- queues[[i]][[j]][["best"]]
            best.like[j] <- queues[[i]][[j]][["best.cost"]]
            print(paste("Best cost for cluster", j, "on core", i, ": ", best.like[j]))
            print(paste("Updated mode for cluster", j, ": ", 
                        paste(out[[j]], collapse = " ")))
          }
        }
      }
    }
    count <- count + 1
    found <- sum(closed) == G*cores
  }
  cat(paste("Algorithm Parallel Search-Pi* Converged in", count, "iterations\n"))
  print(out)
  return(out)
}