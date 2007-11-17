\name{res.eyefit.Hg_O}
\alias{res.eyefit.Hg_O}
\docType{data}
\title{Result of the function eyefit for variogram estimation.}
\description{
This result could also be directly computed using the function eyefit.
}
\usage{data(res.eyefit.Hg_O)}
\format{
  The format is:
List of 1
 $ :List of 7
  ..$ cov.model: chr "exponential"
  ..$ cov.pars : num [1:2] 1.50e-02 3.21e+04
  ..$ nugget   : num 0.04
  ..$ kappa    : num 0.5
  ..$ lambda   : num 0
  ..$ trend    : chr "cte"
  ..$ max.dist : num 288460
  ..- attr(*, "class")= chr "variomodel"
 - attr(*, "class")= chr "eyefit"
}
\examples{
data(res.eyefit.Hg_O)
str(res.eyefit.Hg_O)
}
\keyword{datasets}
