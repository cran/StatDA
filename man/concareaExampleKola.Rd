\name{concareaExampleKola}
\alias{concareaExampleKola}
\title{ Concentration Area Plot for Kola data example}
\description{
  Displays a concentration area plot example for the Kola data. This procedure ist useful for determining if mulitple populations that are spatially dependent are present in a data set. For a more general function see concarea.
}
\usage{
concareaExampleKola(x, y, z, zname = deparse(substitute(z)),
caname = deparse(substitute(z)), borders="bordersKola", logx = FALSE, ifjit = FALSE,
ifrev = FALSE, ngrid = 100, ncp = 0, xlim = NULL, xcoord = "Easting",
ycoord = "Northing", ifbw = FALSE, x.logfinetick = c(2, 5, 10),
y.logfinetick = c(2, 5, 10))
}
\arguments{
  \item{x}{ name of the x-axis spatial coordinate, the eastings }
  \item{y}{ name of the y-axis spatial coordinate, the northings }
  \item{z}{ name of the variable to be processed and plotted }
  \item{zname}{ a title for the x-axes of the qp-plot and concentration area plot. }
  \item{caname}{ a title for the image of interpolated data. }
  \item{borders}{ either NULL or character string with the name of the list with list elements x and
 y for x- and y-coordinates of map borders }
  \item{logx}{ if it is required to make a logarithmis data transformation for the interpolation }
  \item{ifrev}{ if FALSE the empirical function ist plotted from highest value to lowest }
  \item{ngrid}{ default value is 100}
  \item{xlim}{ the range for the x-axis }
  \item{xcoord}{ a title for the x-axis, defaults to "Easting" }
  \item{ycoord}{ a title for the y-axis, defaults to "Northing" }
  \item{ifbw}{ if the plot is drawn in black and white }
  \item{x.logfinetick}{ how fine are the tick marks on log-scale on x-axis }
  \item{y.logfinetick}{ how fine are the tick marks on log-scale on y-axis }
  \item{ifjit}{default value is FALSE}
  \item{ncp}{default value is 0}
}
\details{
  The function assumes that the area is proportional to the count of grid points. To be a reasonable model the data points should be 'evenly' spread over the plane.
  The interpolated grid size ist computed as (max(x) - min(x))/ngrid, with a default value of 100 for ngrid.
  Akima's interpolation function is used to obtain a linear interpolation between the spatial data values.
}
\value{
  An example concentration area plot for Kola is created.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\seealso{ \code{\link{qpplot.das}}, \code{\link{concarea}} }
\examples{
data(ohorizon)
data(kola.background)
data(bordersKola)

Cu=ohorizon[,"Cu"]
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]


op <- par(mfrow=c(2,2),mar=c(1.5,1.5,1.5,1.5))
concareaExampleKola(X,Y,Cu,log=TRUE,zname="Cu in O-horizon [mg/kg]",
   x.logfinetick=c(2,5,10),y.logfinetick=c(10))
par(op)	
}
\keyword{dplot}
