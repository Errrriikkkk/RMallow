\name{genRanks}
\alias{genRanks}
\title{Generate a list of objects of class ranking.}
\usage{
  genRanks(ranks, old.modes, new.modes, orig)
}
\arguments{
  \item{ranks}{a list of objects of class 'ranking'.  May
  be length 1.}

  \item{orig}{the original ranking object, for context.}

  \item{modes}{a matrix of modal sequences}

  \item{updates}{which items in the list do we need to
  update?}
}
\value{
  a list of objects of class 'ranking', one for each mode
}
\description{
  Generate a list of objects of class ranking.
}

