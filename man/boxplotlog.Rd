\name{boxplotlog}
\alias{boxplotlog}
\title{ Boxplotlog }
\description{
  The function boxplot plots a boxplot of the data with respect to the logarithmic transformed values of the whiskers. See also details.
}
\usage{
boxplotlog(x, ..., range = 1.5, width = NULL, varwidth = FALSE, notch = FALSE,
outline = TRUE, names, plot = TRUE, border = par("fg"), col = NULL, log = "",
pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5), horizontal = FALSE,
add = FALSE, at = NULL)
}
\arguments{
  \item{x}{ data }
  \item{\dots}{ further arguments for creating the list }
  \item{range}{ this determines how far the plot "whiskers" extend from the box. If range is positive, the most extreme data point which is no more than range times the length of the box away from the box. A value of zero causes the whiskers to extend to the data extremes. }
  \item{width}{ a vector giving the relative widths of the boxes making up the plot }
  \item{varwidth}{ if varwidth is TRUE, the boxes are drawn with widths proportional to the square-roots of the number of observations in the groups. }
  \item{notch}{ if notch is TRUE, a notch is drawn in each side of the boxes }
  \item{outline}{ if outline is FALSE, the outliers are not drawn }
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
  Sometimes a boxplot of the original data does not identify outliers because the boxplot assumes normal distribution. Therefore the data are logarithmically transformed and values for plotting the boxplot are calculated. After that the data are backtransformed and the boxplot is plotted with respect to the logarithmic results. Now the outliers are identified.
}
\value{
  \item{stats}{a vector of length 5, containing the extreme of the lower whisker, the lower "hinge", the median, the upper "hinge" and the extreme of the upper whisker (backtransformed)}
  \item{n}{the number of non-NA observations in the sample}
  \item{conf}{the lower and upper extremes of the "notch"}
  \item{out}{the values of any data points which lie beyond the extremes of the whiskers (backtransformed)}
  \item{group}{the group}
  \item{names}{the attributes}
  Returns a boxplot which is calculated with the log-transformed data.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(chorizon)
Ba=chorizon[,"Ba"]

boxplotlog((Ba),horizontal=TRUE,xlab="Ba [mg/kg]",cex.lab=1.4,pch=3,cex=1.5)
}
\keyword{ univar }
