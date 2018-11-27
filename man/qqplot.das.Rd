\name{qqplot.das}
\alias{qqplot.das}
\title{ QQ plot}
\description{
  A QQ (Quantile-Quantile) plot is produced.
}
\usage{
qqplot.das(x, distribution = "norm", ylab = deparse(substitute(x)), 
     xlab = paste(distribution, "quantiles"), main = "", las = par("las"), 
     datax = FALSE, envelope = 0.95, labels = FALSE, col = palette()[2], 
     lwd = 2, pch = 1, line = c("quartiles", "robust", "none"), cex = 1, 
     xaxt = "s", add.plot=FALSE,xlim=NULL,ylim=NULL,...)
}
\arguments{
  \item{x}{ numeric vector }
  \item{distribution}{ name of the comparison distribution }
  \item{ylab}{ label for the y axis (empirical quantiles) }
  \item{xlab}{ label for the x axis (comparison quantiles) }
  \item{main}{ title for the plot }
  \item{las}{ if 0, ticks labels are drawn parallel to the axis }
  \item{datax}{ if TRUE, x and y axis are exchanged }
  \item{envelope}{ confidence level for point-wise confidence envelope, or FALSE for no envelope }
  \item{labels}{ vector of point labels for interactive point identification, or FALSE for no labels }
  \item{col, lwd, pch, cex, xaxt }{ graphical parameter, see par }
  \item{line}{ "quartiles" to pass a line through the quartile-pairs, or "robust" for a robust-regression line. "none" suppresses the line }
  \item{add.plot}{ if TRUE the new plot is added to an old one }
  \item{xlim}{ the range for the x-axis}
  \item{ylim}{ the range for the y-axis}
  \item{\dots}{ further arguments for the probability function}
}
\details{
The probability of the input data is computed and with this result the quantiles of the comparison distribution are calculated. If line="quartiles" a line based on quartiles is plotted and if line="robust" a robust LM model is calculated.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\seealso{ \code{\link{par}} }
\examples{
data(AuNEW)
qqplot.das(AuNEW,distribution="lnorm",col=1,envelope=FALSE,datax=TRUE,ylab="Au",
xlab="Quantiles of lognormal distribution", main="",line="none",pch=3,cex=0.7)
}
\keyword{dplot}
