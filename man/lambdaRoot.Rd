\name{lambdaRoot}
\alias{lambdaRoot}
\title{The objective function to find the zero of to find the lambda parameter of the model.}
\usage{
  lambdaRoot(x, N, D, geqties, abils)
}
\arguments{
  \item{x}{The current guess at lambda}

  \item{N}{Number of subjects probably in the cluster}

  \item{D}{The observed distance of the subjects in the
  cluster}

  \item{geqties}{Tie group information for the individuals
  assigned to the cluster.}

  \item{abils}{The number of items ranked in the data.}
}
\value{
  The value of the objective function with the input
  parameters.  Goal is zero.
}
\description{
  The objective function to find the zero of to find the
  lambda parameter of the model.
}

