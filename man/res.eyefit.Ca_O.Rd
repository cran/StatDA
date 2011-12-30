\name{res.eyefit.Ca_O}
\alias{res.eyefit.Ca_O}
\docType{data}
\title{Result of the function eyefit for variogram estimation.}
\description{
This result could also be directly computed using the function eyefit.
}
\usage{data(res.eyefit.Ca_O)}
\format{
  The format is:
List of 1
 $ :List of 7
  ..$ cov.model: chr "spherical"
  ..$ cov.pars : num [1:2]    0.01 5341.85
  ..$ nugget   : num 0.12
  ..$ kappa    : num 0.5
  ..$ lambda   : num 0
  ..$ trend    : chr "cte"
  ..$ max.dist : num 192306
  ..- attr(*, "class")= chr "variomodel"
 - attr(*, "class")= chr "eyefit"
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
data(res.eyefit.Ca_O)
str(res.eyefit.Ca_O)
}
\keyword{datasets}
