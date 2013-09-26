#' Fits a Multi-Modal Mallows' model to ranking data.
#' 
#' Fits the Multi-Modal Mallows' model to partial or full ranking data, using
#' Kendall's metric and an EM algorithm.  This is essentially metric sequence
#' clustering.
#' 
#' @param ranks An object of class "ranking", as provided by this package.
#' @param G Number of modes, 2 or greater.
#' @param iter Maximum number of iterations.
#' @param hyp Hypothesis sequence vector, to initialize one of the cluster
#' centers at.
#' @param threshold difference in likelihood that is accepted to mean the algorithm has converged
#' @param algo a string, either "fv" for Fligner-Verducci or any other string for optimal
#' @param top.bound A vector of top bounds for each cluster
#' @param cores number of cores to use in the search
#' @param local if using algo = "fv", should a local search be performed around the mode?
#' @return an object of class 'mallow'
#' @author Erik Gregory
#' @references "Mixtures of distance-based models for ranking data". Thomas 
#' Brendan Murphy & Donal Martin. 1 April 2002. Computational Statistics & 
#' Data Analysis 41 (2003) 645-655.
#' @keywords cluster Mallow

Mallows <-
  function(ranks, G = 1, iter = 10, hyp = NULL,
           top.bound = rep(1000, ncol(ranks(ranks))), cores = 1,
           threshold = 0.001, algo = "optimal",
           local = FALSE) {
    if(class(ranks) != "ranking") {
      cat("Converting input to a ranking object\n")
      ranks <- ranking$new(ranks)
    }
    orig <- ranks$ranks
    orig1 <- ranks
    N <- ranks$rankers # Number of subjects
    abils <- ranks$items # Number of items being ranked.
    p <- rep(1/G, G) # Initialize the p-value of membership in each cluster.
    like <- -1e10 # Initialize likelihood
    modes <- rGen(G, hyp, abils) # Initialize the modal sequences.
    lambda <- matrix(rep(1, (abils - 1)*G), nrow = G, byrow = TRUE) # Initialize the lambda matrix
    ranks <- genRanks(ranks, t(apply(modes, 1, rev)), modes, orig) # Initialize the ranking objects for each mode
    likelihood <- 0*(1:iter) # track the likelihood
    dists <- dTheta(ranks)
    for (i in 1:iter) {
      z <- expect(ranks, thetas = lambda, p = p)
      if (algo == "fv") {
        lambda <- NULL
        R <- list()
        for (j in 1:G) {
          res <- fv(orig1, z[, j], type = "classic")
          R[[j]] <- res[[1]]
          thet <- res[[2]]
          lambda <- rbind(lambda, thet)
        }
      }
      else {
        Qs <- lapply(1:G, function(j) conQ(orig1, z[, j]))
        R <- parallelSearch(Qs, lambda, cores = cores)
        lambda <- findLambda(ranks, top.bound, z) # TODO: Should I use orig1 here???
      }
      ranks <- genRanks(ranks, modes, R, orig)
      modes <- do.call('rbind', R)
      z <- expect(ranks, thetas = lambda, p = p)
      p <- colSums(z)/N
      likelihood[i] <- likelihoodGMM(p, lambda, ranks)
      print(likelihood[1:i])
      if (i > 1 && abs(likelihood[i] - likelihood[i - 1]) < threshold) {
        cat('EM Algorithm converged\n')
        break
      }
    }
    out <- mallow$new(ranks = orig, modes = modes, theta = lambda, pvals = z,
                      props = p, log.like = likelihood)
  }
