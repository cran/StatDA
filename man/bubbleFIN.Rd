\name{bubbleFIN}
\alias{bubbleFIN}
\title{ Bubbleplot due to Finnish method }
\description{
  This function plots multivariate data with respect to the value. The size of the bubble represents the value of the datapoint.
}
\usage{
bubbleFIN(x, y, z, radi = 10000, S = 9, s = 0.9, wa = 0, wb = 0.95, wc = 0.05,
plottitle = "BubblePlot", legendtitle = "Legend", text.cex = 1,
legtitle.cex = 1, backgr = "kola.background", leg = TRUE, ndigits = 1)
}
\arguments{
  \item{x}{ x coordinates }
  \item{y}{ y coordinates }
  \item{z}{ measured value at point (x,y) }
  \item{radi}{ scaling for the map }
  \item{S, s}{ control the size of the largest and smallest bubbles }
  \item{wa, wb, wc}{ factors which defines the shape of the exponential function }
  \item{plottitle}{ the titel of the plot }
  \item{legendtitle}{ the titel of the legend }
  \item{text.cex}{ multiplier for the size of the labels }
  \item{legtitle.cex}{ multiplier for the size of the legendtitle }
  \item{backgr}{ which background should be used }
  \item{leg}{ if TRUE the bubbles are plotted to the legend }
  \item{ndigits}{ how much digits should be plotted at the legend }
}
\details{
  The smallest bubbles represent the 10\% quantile and the biggest bubbles represent the 99% quantile. The dots, corresponding to all values in between, grow exponentially, where wa, wb and wc are the parameters for the shape.
}
\value{
  Plots bubbles in the existing plot.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
data(kola.background)
data(ohorizon)
el=ohorizon[,"Mg"]
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n") #plot bubbles with background
plotbg(map.col=c("gray","gray","gray","gray"),add.plot=TRUE)

bubbleFIN(X,Y,el,S=9,s=2,plottitle="",legendtitle="Mg [mg/kg]", text.cex=0.63,legtitle.cex=0.80)
}
\keyword{ multivariate }
