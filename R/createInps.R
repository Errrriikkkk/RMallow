#' Create inputs for a parallel searchPi
#' @param Qs list of transition matrices
#' @param thetas matrix of weights for each cluster
#' @param cores number of cores to use in the computation
#' @param len the hypothesized maximum length of any of the queues during the search
#' @return searchable objects to run SearchPi on
createInps <- function(Qs, thetas, cores = 4, len = 200000) {
  G <- nrow(thetas) # number of clusters
  n <- nrow(Qs[[1]]) # number of items ranked
  L <- rep(1000, len) # inititalize lower bound on cost
  L1 <- L
  ind <- vector(cores, mode = "list")
  out <- list()
  j <- 0
  for(i in 1:n) {
    j <- (j %% cores) + 1
    ind[[j]] <- c(ind[[j]], i)
  }
  for (k in 1:G) {
    # assign initial items to each core
    for(i in 1:cores) {
      N <- length(ind[[i]])
      if(k == 1) {
        out[[i]] <- list()
      }
      queue <- vector(mode = "list", len) # create the queue
      queue[1:N] <- as.vector(ind[[i]], mode = "list") # fill in the queue
      L1 <- L
      L1[1:N] <- colSums(Qs[[k]][, ind[[i]]])*thetas[k, 1] # Initial cost
      C <- L1 # Fill in initial cost
      out[[i]][[k]] <- list(queue = queue, L = L1, C = C,
                            pos = N + 1, thetas = thetas[k, ], 
                            closed = FALSE, LUB = 100000,
                            best = NULL)
    }
  }
  return(out)
}