\name{plotuniout}
\alias{plotuniout}
\title{ Multivariate outlier plot for each dimension }
\description{
   A multivariate outlier plot for each dimension is produced.
}
\usage{
plotuniout(x, symb = FALSE, quan = 1/2, alpha = 0.025, bw = FALSE,
pch2 = c(3, 1), cex2 = c(0.7, 0.4), col2 = c(1, 1), lcex.fac = 1, ...)
}
\arguments{
  \item{x}{ dataset }
  \item{symb}{ if FALSE, only two different symbols (outlier and no outlier) will be used }
  \item{quan}{ Number of subsets used for the robust estimation of the covariance matrix. Allowed are values between 0.5 and 1., see covMcd }
  \item{alpha}{ Maximum thresholding proportion, see arw }
  \item{bw}{ if TRUE, symbols are in gray-scale (only if symb=TRUE) }
  \item{pch2, cex2, col2}{ graphical parameters for the points }
  \item{lcex.fac}{ factor for multiplication of symbol size (only if symb=TRUE)}
  \item{\dots}{ further graphical parameters for the plot}
}
\value{
  \item{o}{returns the outliers}
  \item{md}{the square root of the Mahalanobis distance}
  \item{euclidean}{the Euclidean distance of the scaled data}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }
\seealso{ \code{\link{arw}}, \code{\link[robustbase]{covMcd}} }
\examples{
data(moss)
el=c("Ag","As","Bi","Cd","Co","Cu","Ni")
dat=log10(moss[,el])

ans<-plotuniout(dat,symb=FALSE,cex2=c(0.9,0.1),pch2=c(3,21))
}
\keyword{ aplot}
\keyword{ univar}
