\docType{data}
\name{elect}
\alias{elect}
\title{1980 APA Presidential Candidate ranking data.}
\format{The format is: int [1:1378, 1:3] 1 1 1 1 2 2 1 1 2 2 ...  - attr(*,
"dimnames")=List of 2 ..$ : chr [1:1378] "1" "2" "3" "6" ...  ..$ : chr
[1:3] "Carter" "Reagan" "Anderson"}
\source{
  The American Psychological Association,
  http://www.electionstudies.org/studypages/1980prepost/1980prepost.htm
}
\description{
  This data is a pre-processed version of the 1980 American
  Psychological Association Presidential candidate ranking
  data.  It has uninformative rankings removed, and values
  pre-simplified into partial rankings.
}
\examples{
#data(elect)
#head(elect)
}
\keyword{datasets}

