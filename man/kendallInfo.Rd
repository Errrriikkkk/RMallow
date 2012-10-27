\name{kendallInfo}
\alias{kendallInfo}
\title{All information used to calculate Kendall's distance.}
\usage{
  kendallInfo(r, inds = NULL)
}
\arguments{
  \item{r}{Matrix of sequences.}

  \item{inds}{Possibly efficiency increase when doing
  repeated calculations, currently not used.}
}
\value{
  Matrix of 0s, 1s, and NAs representing pairwise
  comparisons of vector values.
}
\description{
  Performs each column-wise comparison on a matrix of
  sequences.  A 0 value denotes that there is an increase
  between the two columns, 1 a decrease, and NA indicates
  that the column values are identical in the row.
}
\author{
  Erik Gregory
}
\references{
  http://en.wikipedia.org/wiki/Kendall_tau_distance
}
\keyword{Distance}
\keyword{Kendall}

