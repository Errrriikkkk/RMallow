\name{expect}
\alias{expect}
\title{Calculate the expected cluster memberships}
\usage{
  expect(ranks, thetas = NULL, p)
}
\arguments{
  \item{ranks}{a list of ranking objects}

  \item{thetas}{a matrix of weights on the metric, one for
  for each cluster}

  \item{p}{the estimated proportions drawn from each
  cluster}
}
\value{
  the expected cluster memberships of each ranker
}
\description{
  Calculate the expected cluster memberships
}

