\name{res.eyefit.AuNEW}
\alias{res.eyefit.AuNEW}
\docType{data}
\title{Result of the function eyefit for variogram estimation.}
\description{
This result could also be directly computed using the function eyefit.
}
\usage{data(res.eyefit.AuNEW)}
\format{
  The format is:
List of 1
 $ :List of 7
  ..$ cov.model: chr "exponential"
  ..$ cov.pars : num [1:2]     0.31 53418.46
  ..$ nugget   : num 0.44
  ..$ kappa    : num 0.5
  ..$ lambda   : num 0
  ..$ trend    : chr "cte"
  ..$ max.dist : num 192306
  ..- attr(*, "class")= chr "variomodel"
 - attr(*, "class")= chr "eyefit"
}
\examples{
data(res.eyefit.AuNEW)
str(res.eyefit.AuNEW)
}
\keyword{datasets}
