\name{Mallows}
\alias{Mallows}
\title{Fits a Multi-Modal Mallows' model to ranking data.}
\usage{
  Mallows(ranks, G = 1, iter = 10, hyp = NULL,
    plot.like = FALSE,
    top.bound = rep(1000, ncol(ranks(ranks))), cores = 1)
}
\arguments{
  \item{ranks}{An object of class "ranking", as provided by
  this package.}

  \item{G}{Number of modes, 2 or greater.}

  \item{iter}{Maximum number of iterations.}

  \item{hyp}{Hypothesis sequence vector, to initialize one
  of the cluster centers at.}

  \item{plot.like}{Should the likelihood be printed at each
  iteration?}

  \item{top.bound}{A vector of top bounds for each cluster}

  \item{cores}{number of cores to use in the search}
}
\value{
  an object of class 'mallow'
}
\description{
  Fits the Multi-Modal Mallows' model to partial or full
  ranking data, using Kendall's metric and an EM algorithm.
  This is essentially metric sequence clustering.
}
\author{
  Erik Gregory
}
\references{
  "Mixtures of distance-based models for ranking data".
  Thomas Brendan Murphy & Donal Martin. 1 April 2002.
  Computational Statistics & Data Analysis 41 (2003)
  645-655.
}
\keyword{cluster}
\keyword{Mallow}

