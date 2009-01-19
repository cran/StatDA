\name{RobCor.plot}
\alias{RobCor.plot}
\title{ Compares the Robust Estimation with the Classical }
\description{
  This function compares a robust covariance (correlation) estimation (MCD is used) with the classical approach. A plot with the two ellipses will be produced and the correlation coefficients are quoted.
}
\usage{
RobCor.plot(x, y, quan = 1/2, alpha = 0.025, colC = 1, colR = 1, ltyC = 2,
ltyR = 1, ...)
}
\arguments{
  \item{x, y}{ two data vectors where the correlation should be computed }
  \item{quan}{ fraction of tolerated outliers (at most 0.5) }
  \item{alpha}{ quantile of chisquare distribution for outlier cutoff }
  \item{colC, colR}{ colour for both ellipses }
  \item{ltyC, ltyR}{ line type for both ellipses }
  \item{\dots}{ other graphical parameters }
}
\details{
The covariance matrix is estimated in a robust (MCD) and non robust way and then both ellipses are plotted. The radi is calculated from the singular value decomposition and a breakpoint (specified quantile) for outlier cutoff.
}
\value{
 \item{cor.cla}{correlation of the classical estimation}
 \item{cor.rob}{correlation of the robust estimation}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
data(chorizon)
Be=chorizon[,"Be"]
Sr=chorizon[,"Sr"]
RobCor.plot(log10(Be),log10(Sr),xlab="Be in C-horizon [mg/kg]",
ylab="Sr in C-horizon [mg/kg]",cex.lab=1.2, pch=3, cex=0.7,
xaxt="n", yaxt="n",colC=1,colR=1,ltyC=2,ltyR=1)
}
\keyword{ univar}
\keyword{ robust}
