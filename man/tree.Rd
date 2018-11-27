\name{tree}
\alias{tree}
\title{ Plot Trees }
\description{
  This function makes a graphical diagram of multivariate data. Every element represents one branch of the tree and the length of the branch indicates the concentration of the element.
}
\usage{
tree(x, wmax = 0, wmin = 0, lh = 1, labels = dimnames(x)[[1]], locations = NULL,
nrow = NULL, ncol = NULL, key.loc = NULL, key.labels = dimnames(x)[[2]],
key.xpd = TRUE, xlim = NULL, ylim = NULL, flip.labels = NULL, len = 1,
leglen = 1, leglh = 1, axes = FALSE, frame.plot = axes, main = NULL, sub = NULL,
xlab = "", ylab = "", cex = 0.8, lwd = 0.25, lty = par("lty"), xpd = FALSE,
mar = pmin(par("mar"), 1.1 + c(2 * axes + (xlab != ""), 2 * axes + (ylab != ""),
 1, 0)), add = FALSE, plot = TRUE, ...)
}
\arguments{
  \item{x}{ multivariate data in form of matrix or data frame }
  \item{wmax, wmin}{ maximum and minimum angle for the leaves of the tree }
  \item{lh}{ multiplier for height }
  \item{labels}{ vector of character strings for labeling the plots }
  \item{locations}{ locations for the boxes on the plot (e.g. X/Y coordinates) }
  \item{nrow, ncol}{ integers giving the numbers of rows and columns to use when location=NULL. By default, 'nrow==ncol', a square layout will be used. }
  \item{key.loc}{ vector with x and y coordinates of the unit key. }
  \item{key.labels}{ vector of character strings for labeling the segments of the unit key. If omitted, the second component of 'dimnames(x)' is used, if available. }
  \item{key.xpd}{ clipping switch for the unit key (drawing and labeling), see 'par("xpd")' }
  \item{xlim}{ vector with the range of x coordinates to plot }
  \item{ylim}{ vector with the range of y coordinates to plot }
  \item{flip.labels}{ logical indication if the label locations should flip up and down from diagram to diagram. Defaults to a somewhat smart heuristic. }
  \item{len, leglen, leglh}{ multiplicative values for the space of the labels on the legend }
  \item{axes}{ logical, if TRUE axes are added to the plot }
  \item{frame.plot}{ if TRUE the plot region is framed }
  \item{main}{ a main title for the plot }
  \item{sub}{ a sub title for the plot }
  \item{xlab}{ a label for the x-axis }
  \item{ylab}{ a label for the y-axis }
  \item{cex}{ character expansion factor for the labels }
  \item{lwd}{ line width used for drawing }
  \item{lty}{ line type used for drawing }
  \item{xpd}{ logical or NA indicating if clipping should be done, see 'par(mar=*)' }
  \item{mar}{ argument to 'par(mar=*)', typically choosing smaller margins than by default. }
  \item{add}{ if TRUE add boxes to current plot }
  \item{plot}{ if FALSE nothing is plotted }
  \item{\dots}{ further arguments, passed to the first call of 'plot()', see 'plot.default' }
}
\details{
Each tree represents one row of the input x. Each branch of the tree represents one choosen element and the length of the branches shows the value of the variable.
The different concentrations of each row in x is displayed by the shape of the tree.
} 
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(ohorizon)
X=ohorizon[,"XCOO"]
Y=ohorizon[,"YCOO"]
el=log10(ohorizon[,c("Co","Cu","Ni","Rb","Bi","Na","Sr")])
sel <- c(3,8,22, 29, 32, 35, 43, 69, 73 ,93,109,129,130,134,168,181,183,205,211,
      218,237,242,276,292,297,298,345,346,352,372,373,386,408,419,427,441,446,490,
      516,535,551,556,558,564,577,584,601,612,617)
x=el[sel,]

tree(x,key.loc=c(15,0),len=0.022, lh=30,leglh=4,
    wmax=120,wmin=30, leglen=0.05, ncol=8, cex=0.75)
}
\keyword{ tree}
\keyword{ multivariate}
