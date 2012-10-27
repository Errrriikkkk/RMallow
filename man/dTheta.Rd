\name{dTheta}
\alias{dTheta}
\title{Calculate Dtheta from a set of rankings to each theoretical modal
sequence of the model.}
\usage{
  dTheta(ranks, thetas = NULL)
}
\arguments{
  \item{ranks}{a list of objects of class 'ranking', or
  something that can be converted to one.}

  \item{thetas}{the spread parameters of the models, in a
  matrix}
}
\value{
  A matrix of the theta distances from each sequence to
  each mode
}
\description{
  Calculate Dtheta from a set of rankings to each
  theoretical modal sequence of the model.
}

