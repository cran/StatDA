\name{suns}
\alias{suns}
\title{ Plot Suns }
\description{
  This function makes a graphical diagram of multivariate data. Every element represents one line in the sun and the length of the line indicates the concentration of the element.
}
\usage{
suns(x, full = TRUE, scale = TRUE, radius = TRUE, labels = dimnames(x)[[1]],
locations = NULL, nrow = NULL, ncol = NULL, len = 1, key.loc = NULL,
key.labels = dimnames(x)[[2]], key.xpd = TRUE, xlim = NULL, ylim = NULL,
flip.labels = NULL, col.stars = NA, axes = FALSE, frame.plot = axes, main = NULL,
sub = NULL, xlab = "", ylab = "", cex = 0.8, lwd = 0.25, lty = par("lty"),
xpd = FALSE,
mar = pmin(par("mar"), 1.1 + c(2 * axes + (xlab != ""), 2 * axes + (ylab != ""), 1, 0)),
add = FALSE, plot = TRUE, ...)
}
\arguments{
  \item{x}{ a matrix or a data frame }
  \item{full}{ if TRUE, a whole circle will be made }
  \item{scale}{ if TRUE, the data will be scaled }
  \item{radius}{ should be TRUE, otherwise the lines in the sun will not be plotted }
  \item{labels}{ the labels for the suns inside the map }
  \item{locations}{ the locations for the suns inside the map }
  \item{nrow, ncol}{ integers giving the number of rows and columns to use when locations=NULL}
  \item{len}{ scaling factor for the length of the lines (according to the size of the map) }
  \item{key.loc}{ the location for the legend }
  \item{key.labels}{ the labels in the legend }
  \item{key.xpd}{ A logical value or NA. If FALSE, all plotting is clipped to the plot region, if TRUE, all plotting is clipped to the figure region, and if NA, all plotting is clipped to the device region. }
  \item{flip.labels}{ logical indication if the label locations should flip up and down from diagram to diagram. }
  \item{axes}{ if FALSE, no axes will be drawn }
  \item{frame.plot}{ if TRUE, a box will be made around the plot }
  \item{main, sub, xlab, xlim, ylim, col.stars, ylab, cex, lwd, lty, xpd, mar}{ graphical parameters and labels for the plot }
  \item{add}{ if TRUE, it will be added to the plot}
  \item{plot}{ nothing is plotted }
  \item{\dots}{ graphical parameters for plotting the box}
}
\details{
Each sun represents one row of the input x. Each line of the sun represents one choosen element. The distance from the center of the sun to the point shows the size of the value of the (scaled) column.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }
\examples{
data(ohorizon)
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
el=log10(ohorizon[,c("Co","Cu","Ni","Rb","Bi","Na","Sr")])

sel <- c(3,8,22, 29, 32, 35, 43, 69, 73 ,93,109,129,130,134,168,181,183,205,211,
      218,237,242,276,292,297,298,345,346,352,372,373,386,408,419,427,441,446,490,
      516,535,551,556,558,564,577,584,601,612,617)
x=el[sel,]
suns(x,ncol=8,key.loc=c(15,0.5),lwd=1.3)
}
\keyword{ multivariate }
\keyword{ dplot}