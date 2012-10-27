\name{topBound}
\alias{topBound}
\title{Calculate the upper bound on the cost to go for the Branch-and-Bound algorithm.}
\usage{
  topBound(thetas, V, j, n, Qmin)
}
\arguments{
  \item{thetas}{see \code{thetas}}

  \item{V}{see \code{V}}

  \item{j}{the current theta index}

  \item{n}{the number of items ranked}

  \item{Qmin}{the minimum value of the Q matrix}
}
\value{
  the top bound on the cost to go
}
\description{
  Calculate the upper bound on the cost to go for the
  Branch-and-Bound algorithm.
}

