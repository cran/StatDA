\name{boxplotperc}
\alias{boxplotperc}
\title{ Boxplot based on percentiles }
\description{
  This function plots a boxplot of the data and the boundaries are based on percentiles.
}
\usage{
boxplotperc(x, ..., quant = c(0.02, 0.98), width = NULL, varwidth = FALSE,
notch = FALSE, outline = TRUE, names, plot = TRUE, border = par("fg"),
col = NULL, log = "", pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
horizontal = FALSE, add = FALSE, at = NULL)
}
\arguments{
  \item{x}{ data }
  \item{\dots}{ further arguments for creating the list}
  \item{quant}{ the underlying percentages }
  \item{width}{ a vector giving the relative widths of the boxes making up the plot }
  \item{varwidth}{ if varwidth is TRUE, the boxes are drawn with widths proportional to the square-roots of the number of observations in the groups. }
  \item{notch}{ if notch is TRUE, a notch is drawn in each side of the boxes }
  \item{outline}{ if outliers is FALSE, the outliers are not drawn }
  \item{names}{ define the names of the attributes }
  \item{plot}{ if plot is TRUE the boxplot is plotted in the current plot }
  \item{border}{ character or numeric (vector) which indicates the color of the box borders }
  \item{col}{ defines the colour }
  \item{log}{ character, indicating if any axis should be drawn in logarithmic scale }
  \item{pars}{ some graphical parameters can be specified }
  \item{horizontal}{ logical parameter indicating if the boxplots should be horizontal; FALSE means vertical boxes }
  \item{add}{ if TRUE the boxplot is added to the current plot }
  \item{at}{ numeric vector giving the locations of the boxplots }
}
\details{
The default value for quant is the 2\% and 98\% quantile and this argument defines the percentiles for the upper and lower whiskers.
}
\value{
  \item{stats}{a vector of length 5, containing the extreme of the lower whisker, the lower "hinge", the median, the upper "hinge" and the extreme of the upper whisker (backtransformed)}
  \item{n}{the number of non-NA observations in the sample}
  \item{conf}{the lower and upper extremes of the "notch"}
  \item{out}{the values of any data points which lie beyond the extremes of the whiskers (backtransformed)}
  \item{group}{the group}
  \item{names}{the attributes}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\seealso{ \code{\link{boxplotlog}} }
\examples{
data(chorizon)
Ba=chorizon[,"Ba"]
boxplotperc(Ba,quant=c(0.05,0.95),horizontal=TRUE,xlab="Ba [mg/kg]",cex.lab=1.2,pch=3)
}
\keyword{univar}
