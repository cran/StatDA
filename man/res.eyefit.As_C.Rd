\name{res.eyefit.As_C}
\alias{res.eyefit.As_C}
\docType{data}
\title{Result of the function eyefit for variogram estimation.}
\description{
This result could also be directly computed using the function eyefit.
}
\usage{data(res.eyefit.As_C)}
\format{
  The format is:
List of 1
 $ :List of 7
  ..$ cov.model: chr "spherical"
  ..$ cov.pars : num [1:2]   0.8 160.3
  ..$ nugget   : num 0.49
  ..$ kappa    : num 0.5
  ..$ lambda   : num 0
  ..$ trend    : chr "cte"
  ..$ max.dist : num 288
  ..- attr(*, "class")= chr "variomodel"
 - attr(*, "class")= chr "eyefit"
}
\examples{
data(res.eyefit.As_C)
str(res.eyefit.As_C)
}
\keyword{datasets}
