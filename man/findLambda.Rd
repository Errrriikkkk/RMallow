\name{findLambda}
\alias{findLambda}
\title{Update the Lambda parameters of each cluster of a Mallows Model.}
\usage{
  findLambda(ranks, top.bound = 1000, z)
}
\arguments{
  \item{ranks}{An object of class "ranking", or a list of
  such objects.}

  \item{top.bound}{If necessary, what is the top bound for
  the lambda parameters?}

  \item{z}{The current probabilities of cluster
  membership.}
}
\value{
  The updated lambda parameters, as a matrix.
}
\description{
  Update the Lambda parameters of each cluster of a Mallows
  Model.
}

