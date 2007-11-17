\name{plotmvoutlier}
\alias{plotmvoutlier}
\title{ Multivariate outlier plot}
\description{
   This function plots multivariate outliers. One possibility is to distinguish between outlier and no outlier. The alternative is to distinguish between the different percentils (e.g. <25\%, 25\%<x<50\%,...).
}
\usage{
plotmvoutlier(coord, data, quan = 1/2, alpha = 0.025, symb = FALSE, bw = FALSE,
plotmap = TRUE, map = "kola.background", which.map = c(1, 2, 3, 4),
map.col = c(5, 1, 3, 4), map.lwd = c(2, 1, 2, 1), pch2 = c(3, 21),
cex2 = c(0.7, 0.2), col2 = c(1, 1), lcex.fac = 1, ...)
}
\arguments{
  \item{coord}{ the coordinates for the points }
  \item{data}{ the value for the different coordinates }
  \item{quan}{ Number of subsets used for the robust estimation of the covariance matrix. Allowed are values between 0.5 and 1., see covMcd }
  \item{alpha}{ Maximum thresholding proportion }
  \item{symb}{ if FALSE, only two different symbols (outlier and no outlier) will be used }
  \item{bw}{ if TRUE, symbols are in gray-scale (only if symb=TRUE) }
  \item{plotmap}{ if TRUE, the map is plotted }
  \item{map}{ the name of the background map }
  \item{which.map, map.col, map.lwd}{ parameters for the background plot, see plotbg }
  \item{pch2, cex2, col2}{ graphical parameters for the points }
  \item{lcex.fac}{ factor for multiplication of symbol size (only if symb=TRUE)}
  \item{\dots}{ further parameters for the plot}
}
\details{
The function computes a robust estimation of the covariance and then the Mahalanobis distances are calculated. With this distances the data set is divided into outliers and non outliers. If symb=FALSE only two different symbols are used otherwise different grey scales are used to distinguish the different types of outliers.
}
\value{
  \item{o}{returns the outliers}
  \item{md}{the square root of the Mahalanobis distance}
  \item{euclidean}{the Euclidean distance of the scaled data}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at>}
\seealso{ \code{\link{plotbg}}, \code{\link[robustbase]{covMcd}}, \code{\link{arw}} }
\examples{
data(moss)
X=moss[,"XCOO"]
Y=moss[,"YCOO"]
el=c("Ag","As","Bi","Cd","Co","Cu","Ni")
x=log10(moss[,el])

data(kola.background)
plotmvoutlier(cbind(X,Y),x,symb=FALSE,map.col=c("grey","grey","grey","grey"),
       map.lwd=c(1,1,1,1),
       xlab="",ylab="",frame.plot=FALSE,xaxt="n",yaxt="n")
}
\keyword{ multivariate}
\keyword{ dplot}
