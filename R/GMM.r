#' Fit a Generalized Mallows Mixture model to a data set
#' @param ranks an object of class 'ranking', or a data set which can 
#' be coerced to such an object.
#' @param G the number of mixture components desired.
#' @param iter the number of iterations to perform before returning
#' the results.  Likelihood stabilization will override if it occurs first.
#' @param hyp a list of hypothesized modal sequences, of length <= G
#' @param cores the number of cores to use in the search for modal sequences
#' @param grouping a variable used to progressively check the clustering progress of the algorithm
#' @param method see further description below.
#' @return An object of class 'mallow', with all information on model fit.
# NOTES: keep a ranking object for each mode in a list,
# and update them only if the modal sequence updates.  
GMM <- function(ranks, G = 1, iter = 10, hyp = NULL, cores = 4, grouping = NULL,
                method = "BNB") {
  ismallow <- ('mallow' %in% class(ranks))
  isranking <- ('ranking' %in% class(ranks))
  if (!isranking & !ismallow) {
    ranks <- new('ranking', ranks)
  }
  orig1 <- ranks
  # Initiate parameters
  N <- rankers(ranks) # Number of rankers
  n <- items(ranks) # Number of items ranked
  # Initial values of dynamic parameters
  orig <- ranks(ranks)
  if (ismallow) { # Continue searching where we left off, if provided
    modes <- modes(ranks)
    thetas <- theta(ranks)
    ps <- props(ranks)
  }
  else { # otherwise initialize parameters somewhat randomly
    modes <- rGen(G, abils = n, hyp = hyp)
    thetas <- matrix(rep(rep(1, n - 1), G), byrow = TRUE,
                     nrow = G)
    ps <- rep(1/G, G)
  }
  ranks <- list()
  ranks <- genRanks(ranks, modes, 1:G, orig)
  like <- 0*(1:iter)
  like2 <- like
  R <- list()
  for(i in 1:iter) {
    z <- expect(ranks, thetas, ps) # E-step
    Qs <- mclapply(1:G, mc.cores = cores, 
                   function(j) conQ(orig1, z[, j]))
    if (G > 1) {
      if (method == "BNB") {
        R <- parallelSearch(Qs, thetas, cores = cores)
      }
      else {
        for(j in 1:G) {
          R[[j]] <- searchPi(Qs[[j]], thetas[j, ])
        }
      }
    }
    else {
      R[[1]] <- searchPi(Qs[[1]], thetas)
    }
    k <- 1
    updates <- list()
    for(j in 1:G) { # update modes
      #R <- #searchPi(Qs, thetas[i, ])
      if(sum(abs(order(R[[j]]) - modes[j, ])) > 0) { # if R changed, we need to create a new ranking object
        updates[[k]] <- j
        print(modes[j, ])
        print(order(R[[j]]))
        modes[j, ] <- order(R[[j]])
        k <- k + 1
      }
    }
    if (length(updates) > 0) {
      updates <- c(do.call('rbind', updates))
      ranks <- genRanks(ranks, modes, updates, orig)
    }
    ps <- colSums(z)/N  # update cluster proportions
    like2[i] <- likelihoodGMM(ps, thetas, ranks)
    oldthetas <- thetas
    thetas <- Theta(ranks, z) #update thetas
    like[i] <- likelihoodGMM(ps, thetas, ranks)
    print(like)
    if (i > 1) {
      if (like[i] < like[i - 1]) {
        print('Likelihood decreased')
        #break
      }
      saved <<- new('mallow', object = new('ranking', orig),
                    modes = modes, theta = thetas, pvals = z, props = ps)
    }
    print(like)
    if(!(is.null(grouping))) {
      blue <- new('mallow', object = new('ranking', orig),
                  modes = modes, theta = thetas, pvals = z, props = ps)
      print(table(grouping, apply(pvals(blue), 1, which.max)))
    }
  }
  return(new('mallow', object = new('ranking', orig),
             modes = modes, theta = thetas, pvals = z, props = ps))
}