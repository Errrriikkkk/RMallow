\name{conQ}
\alias{conQ}
\title{Construct the weighted transitions matrix.}
\usage{
  conQ(ranks, pvals = 1)
}
\arguments{
  \item{ranks}{an object of class 'ranking'}

  \item{pvals}{a vector of probabilities to weight the
  transitions with for each individual.}
}
\value{
  a list of transitions matrices, one for each input in the
  list \code{ranks}
}
\description{
  Construct the weighted transitions matrix.
}

