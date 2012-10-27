\name{mallow-class}
\Rdversion{1.1}
\docType{class}
\alias{mallow-class}
\alias{clust,mallow-method}
\alias{dists,mallow-method}
\alias{initialize,mallow-method}
\alias{log.like,mallow-method}
\alias{modes,mallow-method}
\alias{props,mallow-method}
\alias{pvals,mallow-method}
\alias{show,mallow-method}
\alias{theta,mallow-method}

\title{Class \code{"mallow"}}
\description{
Class to capture the structure of any variant of Mallows's Model
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("mallow", object, modes, props, theta, log.like, clust, dists, pvals)}.
}
\section{Slots}{
  \describe{
    \item{\code{ranks}:}{Object of class \code{"data.frame"} ~~ }
    \item{\code{info}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{type}:}{Object of class \code{"character"} ~~ }
    \item{\code{ties}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{geqties}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{relative}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{V}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{items}:}{Object of class \code{"integer"} ~~ }
    \item{\code{rankers}:}{Object of class \code{"integer"} ~~ }
    \item{\code{modes}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{props}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{theta}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{log.like}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{clust}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{dists}:}{Object of class \code{"matrix"} ~~ }
    \item{\code{pvals}:}{Object of class \code{"matrix"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{ranking}"}, directly.
Class \code{"\linkS4class{modes}"}, directly.
Class \code{"\linkS4class{props}"}, directly.
Class \code{"\linkS4class{theta}"}, directly.
Class \code{"\linkS4class{log.like}"}, directly.
Class \code{"\linkS4class{clust}"}, directly.
Class \code{"\linkS4class{dists}"}, directly.
Class \code{"\linkS4class{pvals}"}, directly.
Class \code{"\linkS4class{ranks}"}, by class "ranking", distance 2.
Class \code{"\linkS4class{info}"}, by class "ranking", distance 2.
Class \code{"\linkS4class{type}"}, by class "ranking", distance 2.
Class \code{"\linkS4class{ties}"}, by class "ranking", distance 2.
Class \code{"\linkS4class{geqties}"}, by class "ranking", distance 2.
Class \code{"\linkS4class{relative}"}, by class "ranking", distance 2.
Class \code{"\linkS4class{V}"}, by class "ranking", distance 2.
Class \code{"\linkS4class{items}"}, by class "ranking", distance 2.
Class \code{"\linkS4class{rankers}"}, by class "ranking", distance 2.
}
\section{Methods}{
  \describe{
    \item{clust}{\code{signature(object = "mallow")}: ... }
    \item{dists}{\code{signature(object = "mallow")}: ... }
    \item{initialize}{\code{signature(.Object = "mallow")}: ... }
    \item{log.like}{\code{signature(object = "mallow")}: ... }
    \item{modes}{\code{signature(object = "mallow")}: ... }
    \item{props}{\code{signature(object = "mallow")}: ... }
    \item{pvals}{\code{signature(object = "mallow")}: ... }
    \item{show}{\code{signature(object = "mallow")}: ... }
    \item{theta}{\code{signature(object = "mallow")}: ... }
	 }
}
\references{
PyMallows
}
\author{
Erik Gregory
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
library("RMallow")
#showClass("mallow")
}
\keyword{classes,mallow}
