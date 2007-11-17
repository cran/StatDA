\name{concarea}
\alias{concarea}
\title{ Plot Contentration Area }
\description{
  Displays a concentration-area plot (see also concareaExampleKola). This function is preferable since it can be applied to non-Kola data!
}
\usage{
concarea(x, y, z, zname = deparse(substitute(z)),
caname = deparse(substitute(z)), borders=NULL, logx = FALSE, ifjit = FALSE,
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
  \item{borders}{ either NULL or character string with the name of the list with list elements x and y for x- and y-coordinates of map borders }
  \item{logx}{ if it is required to make a logarithmis data transformation for the interpolation }
  \item{ifrev}{ if FALSE the empirical function ist plotted from highest value to lowest }
  \item{ngrid}{ default value is 100 }
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
  The concentration area plot, in both directions, is created.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear.  }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }
\seealso{ \code{\link[rgr]{caplot}}, \code{\link{concareaExampleKola}} }
\examples{
data(ohorizon)
data(kola.background)
data(bordersKola)

Cu=ohorizon[,"Cu"]
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]

par(mfrow=c(1,2),mar=c(4,4,2,2))
concarea(X,Y,Cu,log=TRUE,zname="Cu in O-horizon [mg/kg]",borders="bordersKola", ifrev=FALSE,
         x.logfinetick=c(2,5,10),y.logfinetick=c(10))
	
}
\keyword{ dplot }
