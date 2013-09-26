\name{tieInfo}
\alias{tieInfo}
\title{Calculate the tie information for a set of rankings.}
\usage{
  tieInfo(ranks)
}
\arguments{
  \item{ranks}{a data frame of rankings, simplified so that
  there are no gaps in the numbers and the minimum rank is
  1}
}
\value{
  A matrix where M[i, j] is the number of items in ranking
  'i' which are ranked 'j'
}
\description{
  Calculate the tie information for a set of rankings.
}

