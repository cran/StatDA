\name{do.ellipses}
\alias{do.ellipses}
\title{ Plot Ellipses}
\description{
  This function plots ellipses according to a covariance matrix
}
\usage{
do.ellipses(acov, pos, ...)
}
\arguments{
  \item{acov}{ the given covariance matrix }
  \item{pos}{ the location of the ellipse }
  \item{\dots}{ further graphical parameter for the ellipses}
}
\value{
  No return value, creates a plot.
}
\details{
  The correlation matrix of the given covariance is computed and the resulting ellipse is plotted. The radi is computed with the singular value decomposition and the cos/sin is calculated for 100 different degrees.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
#internal function, used in CorCompare
}
\keyword{ aplot }
\keyword{ univar }
