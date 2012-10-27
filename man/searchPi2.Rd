\name{searchPi2}
\alias{searchPi2}
\title{Perform a search for a fixed number of iterations for a modal sequence.}
\usage{
  searchPi2(Qs, thetas, L, C, iter = 1000, pos, queue,
    best = NULL, LUB = 1e+05)
}
\arguments{
  \item{queue}{is the search frontier}

  \item{L}{is the lower-bound on the total cost for the
  sequences in \code{queue}}

  \item{C}{is the incurred cost of each sequence in queue.}

  \item{thetas}{is a vector of weights}

  \item{Qs}{is the transitions matrix of the data}

  \item{iter}{is the number of seqs to test before
  returning to the main loop with results.}

  \item{pos}{is the last non-null position in the queue
  plus 1}

  \item{best}{the best sequence found so far}

  \item{LUB}{the least upper bound on the final cost}
}
\value{
  an updated search object
}
\description{
  Perform a search for a fixed number of iterations for a
  modal sequence.
}

