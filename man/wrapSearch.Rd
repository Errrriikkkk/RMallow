\name{wrapSearch}
\alias{wrapSearch}
\title{A wrapper for the parallel function \code{searchPi2}}
\usage{
  wrapSearch(Qs, thetas, inp, closed, iter = 1000, LUB)
}
\arguments{
  \item{Qs}{list of Q matrices}

  \item{thetas}{matrix of theta values}

  \item{inp}{the other information used for the search}

  \item{closed}{a vector representing which clusters are
  open and closed}

  \item{iter}{the number of iterations before returning to
  the parent cluster}

  \item{LUB}{a vector of the Least Upper Bounds on the
  cost-to-go for each cluster}
}
\value{
  an updated search object
}
\description{
  A wrapper for the parallel function \code{searchPi2}
}

