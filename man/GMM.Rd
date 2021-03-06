\name{GMM}
\alias{GMM}
\title{Fit a Generalized Mallows Mixture model to a data set}
\usage{
  GMM(ranks, G = 1, iter = 10, hyp = NULL, cores = 4,
    grouping = NULL, method = "BNB", algo = "optimal",
    threshold = 0.001, local = FALSE)
}
\arguments{
  \item{ranks}{an object of class 'ranking', or a data set
  which can be coerced to such an object.}

  \item{G}{the number of mixture components desired.}

  \item{iter}{the number of iterations to perform before
  returning the results.  Likelihood stabilization will
  override if it occurs first.}

  \item{hyp}{a list of hypothesized modal sequences, of
  length <= G}

  \item{cores}{the number of cores to use in the search for
  modal sequences}

  \item{grouping}{a variable used to progressively check
  the clustering progress of the algorithm}

  \item{algo}{string supplying the algorithm to use}

  \item{threshold}{difference in the likelihood after which
  to consider the model to have converged}

  \item{method}{see further description below.}

  \item{local}{should a local search be done around they
  hypothesized mode, if using algo = "fv"}
}
\value{
  An object of class 'mallow', with all information on
  model fit.
}
\description{
  Fit a Generalized Mallows Mixture model to a data set
}

