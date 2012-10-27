\name{Theta}
\alias{Theta}
\title{Update the theta parameters of a GMM for each cluster.}
\usage{
  Theta(ranks, z, min.pref = 0.1)
}
\arguments{
  \item{ranks}{A list of ranking objects, possibly of
  length 1.}

  \item{z}{the ranker-wise cluster probabilities}

  \item{min.pref}{the minimum theta weight which can be
  used}
}
\value{
  a matrix of the theta parameters of each cluster
}
\description{
  Update the theta parameters of a GMM for each cluster.
}

