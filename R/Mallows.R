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
#' @param plot.like Should the likelihood be printed at each iteration?
#' @param top.bound A vector of top bounds for each cluster
#' @param cores number of cores to use in the search
#' @return an object of class 'mallow'
#' @author Erik Gregory
#' @references "Mixtures of distance-based models for ranking data". Thomas 
#' Brendan Murphy & Donal Martin. 1 April 2002. Computational Statistics & 
#' Data Analysis 41 (2003) 645-655.
#' @keywords cluster Mallow

Mallows <-
  function(ranks, G = 1, iter = 10, hyp = NULL, plot.like = FALSE, 
           top.bound = rep(1000, ncol(ranks(ranks))), cores = 1) {
    if(class(ranks) != "ranking") {
      cat("Converting input to a ranking object\n")
      ranks <- new("ranking", ranks)
    }
    orig <- ranks(ranks)
    orig1 <- new('ranking', orig)
    N <- rankers(ranks) # Number of subjects
    abils <- items(ranks) # Number of items being ranked.
    p <- rep(1/G, G) # Initialize the p-value of membership in each cluster.
    like <- -1e10 # Initialize likelihood
    modes <- rGen(G, hyp, abils) # Initialize the modal sequences.
    lambda <- matrix(rep(1, (abils - 1)*G), nrow = G, byrow = TRUE) # Initialize the lambda matrix
    # Normalizing Coefficients on lambda
    ranks <- list()
    ranks <- genRanks(ranks, modes, 1:G, orig) # Initialize the ranking objects for each mode
    likelihood <- 0*(1:iter) # track the likelihood
    best.like <- 0
    i <- 1
    R <- list()
    dists <- dTheta(ranks)
    while (i <= iter) {
      if( i > 1) {
        z0 <- z
      }
      z <- expect(ranks, thetas = lambda, p = p, type = "classic")
      Qs <- lapply(1:G, function(j) conQ(orig1, z[, j]))
      if (G > 1) {
        R <- parallelSearch(Qs, lambda, cores = cores)
      }
      else {
        R[[1]] <- matrix(searchPi(Qs[[1]], lambda), nrow = 1)
      }
      modes <- do.call('rbind', R)
      ranks <- genRanks(ranks, modes, 1:G, orig)
      p <- colSums(z)/N
      lambda <- findLambda(ranks, top.bound, G, N, z)
      lambda <- matrix(do.call('rbind', 
                               lapply(lambda, function(i) rep(i, abils - 1))), nrow = G)
#       like0 <- like
#       like <- Likelihood(z, p, C.lam, lambda, all.dists.data, ranking)
#       likelihood[i] <- sum(like)
#       #print(paste("Lambda", likelihood[i]))
#       if (i > 2) {
#         if (likelihood[i] - likelihood[i -1] < 0.001) {
#           cat(paste("Algorithm converged at iteration", i, "\n"))
#           i <- iter
#         }
#         if(likelihood[i] - likelihood[i - 1] < -0.0001) {
#           #i <- iter + 1
#           print(paste("Going Backward...", i))
#         }
#       }
      i <- i + 1
    }
    return(new('mallow', object = new('ranking', orig),
               modes = modes, theta = lambda, pvals = z, props = p))
  }
