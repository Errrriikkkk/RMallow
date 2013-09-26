\name{clusterLikelihood}
\alias{clusterLikelihood}
\title{Find the likelihood of a single cluster of a Mallows mixture model}
\usage{
  clusterLikelihood(ranks, thetas, z)
}
\arguments{
  \item{ranks}{an object of class "ranking"}

  \item{thetas}{the theta matrix for the model}

  \item{z}{the latent variables, where z[i] is the
  probability that ranking i is a member of this cluster}
}
\value{
  the sum of the probabilities that these rankings belong
  to this model
}
\description{
  Find the likelihood of a single cluster of a Mallows
  mixture model
}

