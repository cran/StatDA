\name{SymbLegend}
\alias{SymbLegend}
\title{ Plot Legend}
\description{
  Plots symbols and Legend on a map. There are two different methods (percentile symbols or boxplot symbols) to display the legend. 
}
\usage{
SymbLegend(X, Y, z, type = "percentile", qutiles = c(0, 0.05, 0.25, 0.75, 0.95, 1),
q = NULL, symbtype = "EDA", symbmagn = 0.8, leg.position = "topright",
leg.title = "", leg.title.cex = 0.8, leg.round = 2, leg.wid = 4, leg.just = "right",
cex.scale = 0.8, xf = 9000, logscale = TRUE, accentuate = FALSE)
}
\arguments{
  \item{X}{ X-coordinates }
  \item{Y}{ Y-coordinates }
  \item{z}{ values on the coordinates }
  \item{type}{ "percentile" for percentile legend, "boxplot" for boxplot legend }
  \item{qutiles}{ considered quantiles if type="percentile" is used }
  \item{q}{ if not NULL, provide manually data points where to break }
  \item{symbtype}{ type of symbols to be used; "EDA", "EDAacc", "EDAacc2", "EDAext", "GSC" or "arbit" }
  \item{symbmagn}{ magnification factor for symbols }
  \item{leg.position}{ position of the legend, either character like "topright" or coordinates }
  \item{leg.title}{ title for legend }
  \item{leg.title.cex}{ cex for legend }
  \item{leg.round}{ round legend to specified digits "pretty" }
  \item{leg.wid}{ width (space in numbers) for legend }
  \item{leg.just}{ how to justify the legend }
  \item{cex.scale}{ cex for text "log-scale" and for boxplot legend - only for type="boxplot" }
  \item{xf}{ x-distance from boxplot to number for legend }
  \item{logscale}{ if TRUE a log scale is used (for boxplot scale) and the log-boxplot is computed }
  \item{accentuate}{ if TRUE, accentuated symbols are used (here only EDA accentuated!) }
}
\details{
It is possible to choose between different methods for calculating the range of the values for the different symbols.

If type="percentile" the pre-determined quantiles of the data are computed and are used to plot the symbols.
If type="boxplot" a boxplot is computed and the values were taken to group the values fot the plot and the legend. In the case that a log scale is used the function boxplotlog is used instead of boxplot.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }
\examples{
data(chorizon)
data(kola.background)
el=chorizon[,"As"]
X=chorizon[,"XCOO"]
Y=chorizon[,"YCOO"]

plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n")
plotbg(map.col=c("gray","gray","gray","gray"),add.plot=TRUE)

SymbLegend(X,Y,el,type="percentile",qutiles<-c(0,0.05,0.25,0.75,0.95,1),symbtype="EDA",
symbmagn=0.8,leg.position="topright",leg.title="As [mg/kg]",leg.title.cex=0.8,leg.round=2,
leg.wid=4,leg.just="right")
}
\keyword{ aplot}
