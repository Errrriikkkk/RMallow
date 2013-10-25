#' Fit a Generalized Mallows Mixture model to a data set
#' @param ranks an object of class 'ranking', or a data set which can 
#' be coerced to such an object.
#' @param G the number of mixture components desired.
#' @param iter the number of iterations to perform before returning
#' the results.  Likelihood stabilization will override if it occurs first.
#' @param hyp a list of hypothesized modal sequences, of length <= G
#' @param cores the number of cores to use in the search for modal sequences
#' @param grouping a variable used to progressively check the clustering progress of the algorithm
#' @param algo string supplying the algorithm to use
#' @param threshold difference in the likelihood after which to consider the model to have converged
#' @param method see further description below.
#' @param local should a local search be done around they hypothesized mode, if using algo = "fv"
#' @return An object of class 'mallow', with all information on model fit.
# NOTES: keep a ranking object for each mode in a list,
# and update them only if the modal sequence updates.  
GMM <- function(ranks, G = 1, iter = 10, hyp = NULL, cores = 4, grouping = NULL,
                method = "BNB", algo = "optimal", threshold = 0.001, local = FALSE) {
  ismallow <- ('mallow' %in% class(ranks))
  isranking <- ('ranking' %in% class(ranks))
  if (!isranking & !ismallow) {
    ranks <- ranking$new(ranks)
  }
  orig1 <- ranks
  # Initiate parameters
  N <- ranks$rankers # Number of rankers
  n <- ranks$items # Number of items ranked
  # Initial values of dynamic parameters
  orig <- ranks$ranks
  if (ismallow) { # Continue searching where we left off, if provided
    modes <- ranks$modes
    thetas <- ranks$theta
    ps <- ranks$props
  }
  else { # otherwise initialize parameters somewhat randomly
    modes <- rGen(G, abils = n, hyp = hyp)
    thetas <- matrix(rep(rep(1, n - 1), G), byrow = TRUE,
                     nrow = G)
    ps <- rep(1/G, G)
  }
  ranks <- genRanks(list(), t(apply(modes, 1, rev)), modes, orig)
  like <- 0*(1:iter)
  R <- list()
  for(i in 1:iter) {
    z <- expect(ranks, thetas, ps) # E-step
    if (algo == "optimal") {
      Qs <- lapply(1:G, function(j) {
        conQ(orig1, z[, j])
      })
      if (method == "BNB") {
        R <- parallelSearch(Qs, thetas, cores = cores)
      }
      else {
        for(j in 1:G) {
          R[[j]] <- searchPi(Qs[[j]], thetas[j, ])
        }
      }
      ranks <- genRanks(ranks, modes, R, orig)
      thetas <- Theta(ranks, z) # TODO: Should we do this with these rankings, or the original one???
    }
    else if (algo == "fv") {
      thetas <- NULL
      R <- list()
      res <- mclapply(
        1:G,
        function(j) {
          fv(orig1, z[, j], local = local)
        }
      )
      for (j in 1:G) {
        R[[j]] <- res[[j]][[1]]
        thet <- res[[j]][[2]]
        thetas <- rbind(thetas, thet)
      }
      ranks <- genRanks(ranks, modes, R, orig)
      modes <- do.call('rbind', R)
    }
    ps <- colSums(z)/N  # update cluster proportions
    like[i] <- likelihoodGMM(ps, thetas, ranks)
    if (i > 1 && abs(like[i] - like[i - 1]) < threshold) {
      break
    }
    print(like[1:i])
  }
  out <- mallow$new(ranks = orig, modes = modes, theta = thetas, pvals = z,
                    props = ps, log.like = like)
  out
}
