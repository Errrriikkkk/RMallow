\name{pRank}
\alias{pRank}
\title{Probabilities of a set of rankings given some modes andspread parameters}
\usage{
  pRank(ranks, thetas)
}
\arguments{
  \item{ranks}{a list of objects of class 'ranking', one
  for each mode.}

  \item{thetas}{a matrix of spread parameters for each
  cluster}
}
\value{
  A matrix where the out[i, j] is the probabilty of ranking
  'i' under model 'j'.
}
\description{
  Probabilities of a set of rankings given some modes
  andspread parameters
}

