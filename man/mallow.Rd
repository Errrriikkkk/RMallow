\name{mallow}
\alias{mallow}
\title{Generate any sort of Mallows model on ranking data.}
\usage{
  mallow(ranks, G = 1, iter = 10, hyp = NULL, cores = 4,
    grouping = NULL, method = "BNB", type = "generalized",
    top.bound = rep(1000, ncol(ranks(ranks))))
}
\arguments{
  \item{ranks}{An object of class 'ranking' or 'mallows',
  or a matrix or data frame that can be converted to such
  an object.}

  \item{G}{The number of mixture components to be used}

  \item{iter}{The number of iterations to try to attain
  convergence}

  \item{hyp}{A list of hypothesized modal sequence(s) to
  try to acheive or find a sequence close to}

  \item{cores}{The number of cores used to perform the
  search, if necessary}

  \item{grouping}{A variable to group the sequences by, if
  there is believed to be an underlying clustering
  variable.  This variable will not be used to cluster.}

  \item{method}{If desired, the branch-and-bound algorithm
  will be used.}

  \item{type}{Should we use a Generalized Mallows model (a
  weighted metric) or a standard Mallows model (each
  transposition of items has the same cost).}

  \item{top.bound}{a vector of the top bounds on the lambda
  parameters, if desired.}
}
\value{
  The model fit
}
\description{
  The currently types of developed Mallows models include
  the standard model on full rankings, the standard model
  on partial rankings, the Mallows mixture on full
  rankings, and the Mallows mixture on partial rankings
  (soon to be Generalized Mallows on full, generalized on
  partial, generalized mixtures on full, and generalized
  mixtures on partial).
}

