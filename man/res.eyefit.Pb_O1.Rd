\name{res.eyefit.Pb_O1}
\alias{res.eyefit.Pb_O1}
\docType{data}
\title{Result of the function eyefit for variogram estimation.}
\description{
This result could also be directly computed using the function eyefit.
}
\usage{data(res.eyefit.Pb_O1)}
\format{
  The format is:
List of 1
 $ :List of 7
  ..$ cov.model: chr "spherical"
  ..$ cov.pars : num [1:2] 1.90e-01 5.13e+05
  ..$ nugget   : num 0.11
  ..$ kappa    : num 0.5
  ..$ lambda   : num 0
  ..$ trend    : chr "cte"
  ..$ max.dist : num 288460
  ..- attr(*, "class")= chr "variomodel"
 - attr(*, "class")= chr "eyefit"
}
\examples{
data(res.eyefit.Pb_O1)
str(res.eyefit.Pb_O1)
}
\keyword{datasets}
