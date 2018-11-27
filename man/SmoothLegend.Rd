\name{SmoothLegend}
\alias{SmoothLegend}
\title{ Plots Smoothing Maps and a Legend}
\description{
   Plots smoothing maps and legend based on continuous or percentile scale.
}
\usage{
SmoothLegend(X, Y, z, resol = 200, type = "percentile", whichcol = "gray",
qutiles = c(0, 0.05, 0.25, 0.5, 0.75, 0.9, 0.95, 1), borders=NULL, leg.xpos.min = 780000,
leg.xpos.max = 8e+05, leg.ypos.min = 7760000, leg.ypos.max = 7870000,
leg.title = "mg/kg", leg.title.cex = 0.7, leg.numb.cex = 0.7, leg.round = 2,
leg.wid = 4, leg.numb.xshift = 70000, leg.perc.xshift = 40000,
leg.perc.yshift = 20000, tit.xshift = 35000)
}
\arguments{
  \item{X}{ X-coordinates }
  \item{Y}{ Y-coordinates }
  \item{z}{ values on the coordinates }
  \item{resol}{ resolution of smoothing }
  \item{type}{ "percentile" for percentile legend; "contin" for continuous grey-scale or colour map }
  \item{whichcol}{ type of color scheme to use: "grey", "rainbow", "rainbow.trunc", "rainbow.inv", "terrain" or "topo" }
  \item{qutiles}{ considered quantiles if type="percentile" is used }
  \item{borders}{ either NULL or character string with the name of the list with list elements x and y for x- and y-coordinates of map borders }
  \item{leg.xpos.min}{ minimum value of x-position of the legend }
  \item{leg.xpos.max}{ maximum value of x-position of the legend }
  \item{leg.ypos.min}{ minimum value of y-position of the legend }
  \item{leg.ypos.max}{ maximum value of y-position of the legend }
  \item{leg.title}{ title for legend }
  \item{leg.title.cex}{ cex for legend title }
  \item{leg.numb.cex}{ cex for legend numbers }
  \item{leg.round}{ round legend to specified digits "pretty" }
  \item{leg.wid}{ width (space in numbers) for legend }
  \item{leg.numb.xshift}{ x-shift of numbers in legend relative to leg.xpos.max }
  \item{leg.perc.xshift}{ x-shift of "Percentile" in legend relative to leg.xpos.min }
  \item{leg.perc.yshift}{ y-shift of "Percentile" in legend relative to leg.ypos.max }
  \item{tit.xshift}{ x-shift of title in legend relative to leg.xpos.max }
}
\details{
First a interpolation is applied using different versions of algorithms from Akima and then all points a distinguished into inside an outside the polygonal region. Now the empirical quantiles for points inside the polygon are computed and then the values are plotted in different scales of the choosen colour.
  ATTENTION: here borders were defined for the smoothing region
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(chorizon)
X=chorizon[,"XCOO"]
Y=chorizon[,"YCOO"]
el=log10(chorizon[,"As"])

# generate plot 
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n")

data(bordersKola) # list with list elements x and y for x- and y-corrdinates of map borders
SmoothLegend(X,Y,el,resol=200,type="contin",whichcol="gray",
    qutiles=c(0,0.05,0.25,0.50,0.75,0.90,0.95,1), borders="bordersKola",
    leg.xpos.min=7.8e5,leg.xpos.max=8.0e5,leg.ypos.min=77.6e5,leg.ypos.max=78.7e5,
    leg.title="mg/kg", leg.title.cex=0.7, leg.numb.cex=0.7, leg.round=2,leg.wid=4,
    leg.numb.xshift=0.7e5,leg.perc.xshift=0.4e5,leg.perc.yshift=0.2e5,tit.xshift=0.35e5)

# plot background
data(kola.background)
plotbg(map.col=c("gray","gray","gray","gray"),map.lwd=c(1,1,1,1),add.plot=TRUE)
}
\keyword{ dplot}
\keyword{ smooth }
