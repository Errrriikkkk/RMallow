\name{findLambda}
\alias{findLambda}
\title{Update the Lambda parameters of each cluster of a Mallows Model.}
\usage{
  findLambda(ranks, top.bound, G, N, z)
}
\arguments{
  \item{ranks}{An object of class "ranking".}

  \item{top.bound}{If necessary, what is the top bound for
  the lambda parameters?}

  \item{G}{Number of clusters in the model.}

  \item{N}{Number of rows in the rankings.}

  \item{z}{The current probabilities of cluster
  membership.}
}
\value{
  The updated lambda parameters, as a vector.
}
\description{
  Update the Lambda parameters of each cluster of a Mallows
  Model.
}

