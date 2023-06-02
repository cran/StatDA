\name{boxes}
\alias{boxes}
\title{ Boxes }
\description{
  The function boxes computes boxes of multivariate data. If add=TRUE the boxes are plotted in the current plot otherwise nothing is plotted.
}
\usage{
boxes(x, xA = 1, yA = 2, zA = 3, labels = dimnames(x)[[1]], locations = NULL,
nrow = NULL, ncol = NULL, key.loc = NULL, key.labels = dimnames(x)[[2]],
key.xpd = TRUE, xlim = NULL, ylim = NULL, flip.labels = NULL, len = 1,
leglen = 1, axes = FALSE, frame.plot = axes, main = NULL, sub = NULL,
xlab = "", ylab = "", cex = 0.8, lwd = 0.25, lty = par("lty"), xpd = FALSE,
mar = pmin(par("mar"), 1.1 + c(2 * axes + (xlab != ""), 2 * axes + (ylab != ""),
 1, 0)), add = FALSE, plot = TRUE, ...)
}
\arguments{
  \item{x}{ multivariate data in form of matrix or data frame }
  \item{xA}{ assignment of clusters to the coordinates of the boxes }
  \item{yA}{ assignment of clusters to the coordinates of the boxes }
  \item{zA}{ assignment of clusters to the coordinates of the boxes }
  \item{labels}{ vector of character strings for labeling the plots }
  \item{locations}{ locations for the boxes on the plot (e.g. X/Y coordinates) }
  \item{nrow}{ integers giving the number of rows ands columns to use when 'locations' is 'NULL'. By default, 'nrow == ncol', a square will be used. }
  \item{ncol}{ integers giving the number of rows and columns to use when 'locations' is 'NULL'. By default, 'nrow == ncol', a square will be used. }
  \item{key.loc}{ vector with x and y coordinates of the unit key. }
  \item{key.labels}{ vector of character strings for labeling the segments of the unit key. If omitted, the second component of 'dimnames(x)' ist used, if available. }
  \item{key.xpd}{ clipping switch for the unit key (drawing and labeling), see 'par("xpd")'. }
  \item{xlim}{ vector with the range of x coordinates to plot }
  \item{ylim}{ vector with the range of y coordinates to plot }
  \item{flip.labels}{ logical indicating if the label locations should flip up and down from diagram to diagram. Defaults to a somewhat smart heuristic. }
  \item{len}{ multiplicative values for the space used in the plot window }
  \item{leglen}{ multiplicative values for the space of the labels on the legend }
  \item{axes}{ logical flag: if 'TRUE' axes are added to the plot }
  \item{frame.plot}{ logical flag: if 'TRUE', the plot region ist framed }
  \item{main}{ a main title for the plot }
  \item{sub}{ a sub title for the plot }
  \item{xlab}{ a label for the x axis }
  \item{ylab}{ a label for the y axis }
  \item{cex}{ character expansion factor for the labels }
  \item{lwd}{ line width used for drawing }
  \item{lty}{ line type used for drawing }
  \item{xpd}{ logical or NA indicationg if clipping should be done, see 'par(xpd=.)' }
  \item{mar}{ argument to 'par(mar=*)', rypically choosing smaller margings than by default }
  \item{add}{ logical, if 'TRUE' add boxes to current plot }
  \item{plot}{ logical, if 'FALSE', nothing is plotted }
  \item{\dots}{ further arguments, passed to the first call of 'plot()' }
}
\value{
  No return value, creates a plot.
}
\details{
  This type of graphical approach for multivariate data is only applicable where the data can be grouped into three clusters. This means that before the plot can be made the data undergo a hierarchical cluster to get the size of each cluster. The distance measure for the hierarchicla cluster is complete linkage. Each cluster represents one side of the boxes.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\seealso{ \code{\link{plot.default}},\code{\link{box}} }
\examples{
#plots the background and the boxes for the elements
data(ohorizon)
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
el=log10(ohorizon[,c("Co","Cu","Ni","Rb","Bi","Na","Sr")])
data(kola.background)

sel <- c(3,8,22, 29, 32, 35, 43, 69, 73 ,93,109,129,130,134,168,181,183,205,211,
      218,237,242,276,292,297,298,345,346,352,372,373,386,408,419,427,441,446,490,
      516,535,551,556,558,564,577,584,601,612,617)

x=el[sel,]
xwid=diff(range(X))/12e4
ywid=diff(range(Y))/12e4
plot(X,Y,frame.plot=FALSE,xaxt="n",yaxt="n",xlab="",ylab="",type="n",
   xlim=c(360000,max(X)))
plotbg(map.col=c("gray","gray","gray","gray"),add.plot=TRUE)

boxes(x,locations=cbind(X[sel],Y[sel]),len=20000,key.loc=c(800000,7830000),leglen=25000,
     cex=0.75, add=TRUE, labels=NULL, lwd=1.1)

}
\keyword{multivariate}

