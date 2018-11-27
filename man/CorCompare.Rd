\name{CorCompare}
\alias{CorCompare}
\title{ Compares Correlation Matrices }
\description{
  This function compares two correlation matrices numerically and graphically.
}
\usage{
CorCompare(cor1, cor2, labels1, labels2, method1, method2, ndigits = 4,
lty1 = 1, lty2 = 2, col1 = 1, col2 = 2, lwd1 = 1.1, lwd2 = 1.1,
cex.label = 1.1, cex.legend = 1.2, lwd.legend = 1.2, cex.cor = 1, ...)
}
\arguments{
  \item{cor1,cor2}{ two correlation matrices based on different estimation methods }
  \item{labels1, labels2}{ labels for the two estimation methods }
  \item{method1, method2}{ description of the estimation methods }
  \item{ndigits}{ number of digits to be used for plotting the numbers }
  \item{lty1, lty2, col1,col2, lwd1, lwd2, cex.label, cex.cor}{ other graphics parameters }
  \item{cex.legend, lwd.legend}{graphical parameters for the legend}
  \item{\dots}{further graphical parameters for the ellipses}
}
\details{
The ellipses are plotted with the function do.ellipses. Therefore the radius is calculated with singular value decomposition.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(chorizon)
x=chorizon[,c("Ca","Cu","Mg","Na","P","Sr","Zn")]
par(mfrow=c(1,1),mar=c(4,4,2,0))
R=robustbase::covMcd(log10(x),cor=TRUE)$cor
P=cor(log10(x))

CorCompare(R,P,labels1=dimnames(x)[[2]],labels2=dimnames(x)[[2]],
method1="Robust",method2="Pearson",ndigits=2, cex.label=1.2)

}
\keyword{univar}
\keyword{multivariate}
