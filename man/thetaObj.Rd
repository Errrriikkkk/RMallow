\name{thetaObj}
\alias{thetaObj}
\title{The objective function used to solve for \code{theta}}
\usage{
  thetaObj(V, n, j, z, relative, theta)
}
\arguments{
  \item{V}{see \code{V}}

  \item{n}{the number of items ranked}

  \item{j}{the current item we are finding the theta value
  for}

  \item{z}{the expected cluster memberships of the data}

  \item{relative}{see \code{relative}}

  \item{theta}{the current guess of \code{theta}}
}
\value{
  the value of the objective function with the given
  parameters.  Goal is zero.
}
\description{
  The objective function used to solve for \code{theta}
}

