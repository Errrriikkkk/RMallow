\name{admissible}
\alias{admissible}
\title{Calculate the admissible heuristic for the SearchPi Algorithm.}
\usage{
  admissible(thetas, V, j, n, Qmax)
}
\arguments{
  \item{thetas}{a vector of theta values}

  \item{V}{a matrix where V[i, j] represents the number of
  items in [j:n] ranked less than or equal to j by ranker
  i}

  \item{j}{the index of the theta value in question}

  \item{n}{the number of theta values, minus 1}

  \item{Qmax}{the maximum value in the Q matrix}
}
\value{
  real number representing the lower bound on the
  cost-to-go
}
\description{
  Calculate the admissible heuristic for the SearchPi
  Algorithm.
}

