\name{polys}
\alias{polys}
\title{ Connect the Values with a Polygon}
\description{
  Connect the values for the elements with a polygon. Every "point" has his own shape and this demonstrates the characteristic of the point.
}
\usage{
polys(x, scale = TRUE, labels = dimnames(x)[[1]], locations = NULL, 
nrow = NULL, ncol = NULL, key.loc = NULL, key.labels = dimnames(x)[[2]], 
key.xpd = TRUE, xlim = NULL, ylim = NULL, flip.labels = NULL, factx = 1, 
facty = 1, col.stars = NA, axes = FALSE, frame.plot = axes, main = NULL, 
sub = NULL, xlab = "", ylab = "", cex = 0.8, lwd = 1.1, lty = par("lty"), 
xpd = FALSE, 
mar = pmin(par("mar"), 1.1 + c(2 * axes + (xlab != ""), 2 * axes + 
      (ylab != ""), 1, 0)), 
add = FALSE, plot = TRUE, ...)
}
\arguments{
  \item{x}{ a matrix or a data frame }
  \item{scale}{ if TRUE, the data will be scaled }
  \item{labels}{ the labels for the polygons inside the map }
  \item{locations}{ the locations for the polygons inside the map }
  \item{nrow,ncol}{ integers giving the number of rows and columns to use when locations=NULL. By default, 'nrow==ncol', a square layout will be used.}
  \item{key.loc}{ the location for the legend }
  \item{key.labels}{ the labels in the legend }
  \item{key.xpd}{ A logical value or NA. If FALSE, all plotting is clipped to the plot region, if TRUE, all plotting is clipped to the figure region, and if NA, all plotting is clipped to the device region. }
  \item{flip.labels}{ logical indicating if the label locations should flip up and down from diagram to diagram. }
  \item{factx}{ additive factor for the x-coordinate}
  \item{facty}{ magnification for the influence of the x-coordinate on the y-coordinate }
  \item{main, sub, xlab, ylab, xlim, ylim, col.stars,cex, lwd, lty, xpd, mar}{ graphical parameters and labels for the plot }
  \item{axes}{ if FALSE, no axes will be drawn }
  \item{frame.plot}{ if TRUE, a box will be made around the plot }
  \item{add}{ if TRUE, it will be added to the plot}
  \item{plot}{ nothing is plotted }
  \item{\dots}{further graphical parameters}
}
\details{
Each polygon represents one row of the input x. For the variables the values are computed and then those values are connected with a polygon. The location of the polygons can be defined by the user.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
data(ohorizon)
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
el=log10(ohorizon[,c("Cu","Ni","Na","Sr")])
sel <- c(3,8,22, 29, 32, 35, 43, 69, 73 ,93,109,129,130,134,168,181,183,205,211,
      218,237,242,276,292,297,298,345,346,352,372,373,386,408,419,427,441,446,490,
      516,535,551,556,558,564,577,584,601,612,617)
x=el[sel,]
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n",
   xlim=c(360000,max(X)))
polys(x,ncol=8,key.loc=c(15,1),factx=0.30,facty=2.0,cex=0.75,lwd=1.1)
}
\keyword{ aplot}
\keyword{ multivariate }
