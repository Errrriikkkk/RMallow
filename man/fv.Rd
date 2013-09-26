\name{fv}
\alias{fv}
\title{Function to run the Fligner-Verducci heuristic on a set of rankings,
as extended to potentially partial rankings on mixtures of
generalized Mallows models.}
\usage{
  fv(ranks, z, type = "generalized", top.bound = 1000,
    local = FALSE)
}
\arguments{
  \item{ranks}{an object of class ranking}

  \item{z}{vector where v[i] represents the probability
  that ranking}

  \item{type}{either "generalized" or not, used to
  determine the type of theta to search for}

  \item{top.bound}{the maximum value of the theta parameter
  in a classic mallows model}

  \item{local}{should the local search around the best
  guess for the mode be performed?}
}
\value{
  the hypothesized modal sequence, theta parameters, and
  likelihood found.
}
\description{
  Function to run the Fligner-Verducci heuristic on a set
  of rankings, as extended to potentially partial rankings
  on mixtures of generalized Mallows models.
}

