\name{simplifySequences}
\alias{simplifySequences}
\title{Change the form of ordered sequences.}
\usage{
  simplifySequences(loss.time)
}
\arguments{
  \item{loss.time}{Matrix of sequences to be simplified.}
}
\value{
  Simplified sequences, as described in Description.
}
\description{
  Simplifies sequences so that each tie group is only of
  distance 1 to the next tie group.  For example, we would
  simplify (1, 1, 2, 4, 4, 5) to (1, 1, 2, 3, 3, 4).
}
\author{
  Erik Gregory
}
\keyword{sequence}
\keyword{simplify}

