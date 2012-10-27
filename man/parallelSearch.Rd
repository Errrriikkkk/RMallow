\name{parallelSearch}
\alias{parallelSearch}
\title{Perform a parallel search for the modes of a Mallows Mixture model on a data set.}
\usage{
  parallelSearch(Qs, thetas, cores = 4, best.worst = 10000,
    iter = 500)
}
\arguments{
  \item{Qs}{a list of transitions matricies, one for each
  cluster}

  \item{thetas}{the matrix of weights on the metric for
  each cluster}

  \item{cores}{the number of cores used to perform the
  search}

  \item{best.worst}{the upper bound on the final cost}

  \item{iter}{the number of iterations after which the
  searchers return to the parent to consolidate findings}
}
\value{
  a list of the modes found, one for each cluster
}
\description{
  Perform a parallel search for the modes of a Mallows
  Mixture model on a data set.
}

