\name{plotellipse}
\alias{plotellipse}
\title{ Plot Ellipse }
\description{
  Plots an ellipse with percentage tolerance and a certain location and covariance.
}
\usage{
plotellipse(x.loc, x.cov, perc = 0.98, col = NULL, lty = NULL)
}
\arguments{
  \item{x.loc}{ the location vector }
  \item{x.cov}{ the covariance }
  \item{perc}{ defines the percentage and should be a (vector of) number(s) between 0 and 1 }
  \item{col, lty}{ graphical parameters }
}
\details{
First the radius of the covariance is calculated and then the ellipses for the provided 
percentages are plotted at the certain location.
}
\value{
  Plot with ellipse.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
data(moss)
Ba=log10(moss[,"Ba"])
Ca=log10(moss[,"Ca"])
plot.new()
plot.window(xlim=range(Ba),ylim=c(min(Ca)-1,max(Ca)))

x=cbind(Ba,Ca)
plotellipse(apply(x,2,mean),cov(x),perc=c(0.5,0.75,0.9,0.98))

}
\keyword{ dplot}
\keyword{ univar}
