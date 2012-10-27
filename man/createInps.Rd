\name{createInps}
\alias{createInps}
\title{Create inputs for a parallel searchPi}
\usage{
  createInps(Qs, thetas, cores = 4, len = 2e+05)
}
\arguments{
  \item{Qs}{list of transition matrices}

  \item{thetas}{matrix of weights for each cluster}

  \item{cores}{number of cores to use in the computation}

  \item{len}{the hypothesized maximum length of any of the
  queues during the search}
}
\value{
  searchable objects to run SearchPi on
}
\description{
  Create inputs for a parallel searchPi
}

