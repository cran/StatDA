\name{varioCalc}
\alias{varioCalc}
\title{ Variogram Calculation}
\description{
  This function calculates (and plots) a variogram.
}
\usage{
varioCalc(X, Y, el, max.dist = 300, title = "", km = TRUE, plot = TRUE)
}
\arguments{
  \item{X}{ X-coordinate }
  \item{Y}{ Y-coordinate }
  \item{el}{ vector or matrix with data values }
  \item{max.dist}{ a numerical value defining the maximum distance for the variogram. All pairs of locations separated for a distance larger than this value are ignored in the variogram calculation. }
  \item{title}{ title for the plot }
  \item{km}{ if TRUE the distances are given in km, otherwise the unit is m }
  \item{plot}{ if TRUE the variogram is plotted, otherwise only the parameters are returned }
}
\details{
A omnivariogram, E-W and N-S variogram is calculated and then the results are plotted.
}
\value{
  \item{vario.b}{a omnidirectional variogram}
}
\seealso{ \code{\link[geoR]{variog}} }
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
data(ohorizon)
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
vario.b=varioCalc(X,Y,el=ohorizon[,"Hg"],max.dist=300000,title=paste("Hg","in O-horizon"),
km=FALSE)
}
\keyword{ dplot}
