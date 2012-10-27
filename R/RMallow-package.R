#' 1980 APA Presidential Candidate ranking data.
#' 
#' This data is a pre-processed version of the 1980 American Psychological
#' Association Presidential candidate ranking data.  It has uninformative
#' rankings removed, and values pre-simplified into partial rankings.
#' 
#' 
#' @name elect
#' @docType data
#' @format The format is: int [1:1378, 1:3] 1 1 1 1 2 2 1 1 2 2 ...  - attr(*,
#' "dimnames")=List of 2 ..$ : chr [1:1378] "1" "2" "3" "6" ...  ..$ : chr
#' [1:3] "Carter" "Reagan" "Anderson"
#' @source The American Psychological Association, http://www.electionstudies.org/studypages/1980prepost/1980prepost.htm
#' @keywords datasets
#' @examples 
#' #data(elect)
#' #head(elect)
NULL





#' Fit Multi-modal Mallows' models to ranking data.
#' 
#' Fits the Mallows' model to ranking data.  Data can be partially or
#' fully-ranked.
#' 
#' \tabular{ll}{ Package: \tab RMallow\cr Type: \tab Package\cr Version: \tab
#' 1.0\cr Date: \tab 2012-02-18\cr License: \tab GPL (>= 2) } 
#' @name RMallow-package
#' @aliases RMallow-package RMallow
#' @docType package
#' @author Erik Gregory
#' Maintainer: <egregory2007@@yahoo.com>
#' @references "Mixtures of distance-based models for ranking data". Thomas 
#' Brendan Murphy & Donal Martin. 1 April 2002. Computational Statistics & 
#' Data Analysis 41 (2003) 645-655.
#'
#' @references "Estimating a Population Distribution of Sequences of k Items from Cross-
#' Sectional Data". Laurel A. Smith (Beckett) and Denis A. Evans. Journal of
#' the Royal Statistical Society.  Series C (Applied Statistics). Vol. 40,
#' No. 1 (1991), pp.31-42.  Blackwell Publishing for the Royal Statistical Society.
#' Accessed 16/08/2010. http://www.jstor.org/stable/2347903 .
#'
#' @references "A Non-iterative procedure for maximum likelihood estimation of the parameters of Mallows' Model Based on Partial Rankings".  Laura Adkins and Michael Flinger. Communication in Statistics - Theory and Methods, 27:9, 2199-2220. 1998, Marchel Dekker, Inc. http://dx.doi.org/10.1080/03610929808832223 .
#' @keywords ranking
NULL













