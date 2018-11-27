\name{edaplot}
\alias{edaplot}
\title{ EDA-plot for data }
\description{
  This function plots a histogram of the data. There is also the choice to add the density, a boxplot and a scatterplot to the histogram.
}
\usage{
edaplot(data,scatter=TRUE,box=TRUE, P.plot=TRUE, D.plot=TRUE,
         P.main=paste("Histogram of",deparse(substitute(data))),
         P.sub=NULL, P.xlab=deparse(substitute(data)), P.ylab=default, P.ann=par("ann"),
         P.axes=TRUE, P.frame.plot=P.axes, P.log=FALSE, P.logfine=c(2,5,10), P.xlim=NULL,
         P.cex.lab=1.4,B.range=1.5, B.notch=FALSE, B.outline=TRUE,
         B.border=par("fg"), B.col=NULL, B.pch=par("pch"), B.cex=1, B.bg=NA, 
         H.breaks="Sturges", H.freq=TRUE, H.include.lowest=TRUE, H.right=TRUE, 
         H.density=NULL, H.angle=45, H.col=NULL, H.border=NULL, H.labels=FALSE, 
         S.pch=".", S.col=par("col"), S.bg=NA, S.cex=1, D.lwd=1,D.lty=1)
}
\arguments{
  \item{data}{ data set }
  \item{scatter}{ if TRUE the scatter plot is added}
  \item{box}{ if TRUE a boxplot or boxplotlog is added}
  \item{P.plot}{ if it is plotted or just a list is computed}
  \item{D.plot}{ if TRUE the density is added}
  \item{P.main, P.sub,P.xlab,P.ylab,P.ann}{ graphical parameters for the density, see plot}
  \item{P.axes,P.frame.plot}{ plots the y-axis with the ticker}
  \item{P.log}{ if TRUE the x-axis is in log-scale}
  \item{P.logfine}{ how fine the tickers are }
  \item{P.xlim,P.cex.lab}{further graphical parameters}
  \item{B.range, B.notch, B.outline,B.border, B.col, B.pch,B.cex, B.bg}{ parameters for boxplot and boxplotlog function, see boxplot and boxplotlog}
  \item{H.breaks, H.freq,H.include.lowest, H.right,H.density,H.angle,H.col,H.border,H.labels}{ parameters for histogram, see hist}
  \item{S.pch, S.col,S.bg,S.cex}{ graphical parameters for the shape of the points, see points }
  \item{D.lwd, D.lty}{ parameters for the density}
}
\details{
First the histogram, boxplot/boxplotlog and density is calculate and then the plot is produced. The default is that histogram, boxplot, density trace and scatterplot is made.
}
\value{
  \item{H}{results of the histogram}
  \item{B}{results of the boxplot}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\seealso{ \code{\link{plot}},\code{\link{boxplot}}, \code{\link{edaplotlog}}, \code{\link{hist}}, \code{\link{points}} }
\examples{
data(chorizon)
Ba=chorizon[,"Ba"]
edaplot(Ba,H.freq=FALSE,box=TRUE,H.breaks=30,S.pch=3,S.cex=0.5,D.lwd=1.5,P.log=FALSE,
  P.main="",P.xlab="Ba [mg/kg]",P.ylab="Density",B.pch=3,B.cex=0.5)
}
\keyword{ univar }
