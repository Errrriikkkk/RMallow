\name{normalize}
\alias{normalize}
\title{Find the normalizing coefficient of a Generalized Mallows model
given the tie information of the data and the spread parameters}
\usage{
  normalize(ranks, thetas = NULL)
}
\arguments{
  \item{ranks}{A list of objects of class 'ranking'}

  \item{thetas}{Matrix of spread parameters, one row for
  each object in the list of ranks}
}
\value{
  A matrix of the normalizing coefficients for the theta
  values
}
\description{
  Find the normalizing coefficient of a Generalized Mallows
  model given the tie information of the data and the
  spread parameters
}

