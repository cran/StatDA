\name{plotvario}
\alias{plotvario}
\title{ Plot Empirical Variogram}
\description{
  Plot sample (empirical) variogram computed using the function variog.
}
\usage{
plotvario(x, max.dist, vario.col = "all",
scaled = FALSE, var.lines = FALSE, envelope.obj = NULL,
pts.range.cex, bin.cloud = FALSE, ...)
}
\arguments{
  \item{x}{ an object of the class "variogram", typically an output of the function variog }
  \item{max.dist}{ maximum distance for the x-axis. The default is the maximum distance for which the sample variogram was computed }
  \item{vario.col}{ only used if obj has information on more than one empirical variogram. The default "all" indicates that variograms of all variables should be plotted. Alternativelly a numerical vector can be used to select variables. }
  \item{scaled}{ If TRUE the variogram values are divided by the sample variance. This allows comparison of variograms of variables measured in different scales. }
  \item{var.lines}{ If TRUE a horizontal line is drawn at the value of the variance of the data (if scaled=FALSE) or at 1 (if scaled=TRUE) }
  \item{envelope.obj}{ adds a variogram envelope }
  \item{pts.range.cex}{ optional. A two elements vector with maximum and minimum values for the character expansion factor cex. If provided the point sizes in binned variogram are proportional to the number of pairs of points used to compute each bin. }
  \item{bin.cloud}{ logical. If TRUE and the sample variogram was computed with the option keep.cloud=TRUE, boxplots of values at each bin are plotted instead of the empirical variograms. }
  \item{\dots}{ other arguments to be passed to the function plot or matplot. }
}
\details{
  Computes the same as the function plot.variogram from the library geoR.
}
\value{
  Variogram plot.
}
\seealso{ \code{\link[geoR]{plot.variogram}} }
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(chorizon)
X=chorizon[,"XCOO"]/1000
Y=chorizon[,"YCOO"]/1000
el=chorizon[,"As"]
vario.b <- variog(coords=cbind(X,Y), data=el, lambda=0, max.dist=300)
plotvario(vario.b,xlab="Distance [km]",ylab="Semivariogram",
cex.lab=1.2,max.dist=300,pch=1,cex=1)
}
\keyword{ multivariate}
\keyword{ dplot}
