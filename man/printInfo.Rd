\name{printInfo}
\alias{printInfo}
\title{Function used to inform user of the status of the search for the modal sequences}
\usage{
  printInfo(Ls, pos, node, cluster, best, LUB)
}
\arguments{
  \item{Ls}{the minimum lower bound used thus far}

  \item{pos}{the number of items in the queue}

  \item{node}{the search node number}

  \item{cluster}{the cluster the node is searching on}

  \item{best}{the best found sequence}

  \item{LUB}{the least upper bound on the final cost for
  the cluster}
}
\value{
  prints out the status of the search
}
\description{
  Function used to inform user of the status of the search
  for the modal sequences
}

