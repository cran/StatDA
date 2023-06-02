\name{ppplot.das}
\alias{ppplot.das}
\title{ PP plot}
\description{
  This function computes a PP (Probability-Probability) plot for the given dataset.
}
\usage{
ppplot.das(x, pdist = pnorm, xlab = NULL, ylab = "Probability", line = TRUE, 
        lwd = 2, pch = 3, cex = 0.7, cex.lab = 1, ...)
}
\arguments{
  \item{x}{ dataset }
  \item{pdist}{ the distribution function }
  \item{xlab, ylab, lwd, pch, cex, cex.lab}{ graphical parameters }
  \item{line}{if a regression line should be added}
  \item{\dots}{ further parameters for the probability function}
}
\value{
  No return value, creates a plot.
}
\details{
The empirical probability is calculated and compared with the comparison distribution.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(AuNEW)
ppplot.das(AuNEW,pdist=plnorm,xlab="Probability of Au",
     ylab="Probabilities of lognormal distribution", pch=3,cex=0.7)
}
\keyword{ dplot}
