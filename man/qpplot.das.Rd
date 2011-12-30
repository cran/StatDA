\name{qpplot.das}
\alias{qpplot.das}
\title{ QP plot}
\description{
  This function produces a QP (Quantile-Probability) plot of the data.
}
\usage{
qpplot.das(x, qdist = qnorm, probs = NULL, logx = FALSE, cex.lab = 1,
xlab = NULL, ylab = "Probability [\%]", line = TRUE, lwd = 2, pch = 3,
logfinetick = c(10), logfinelab = c(10), cex = 0.7, xlim = NULL,
ylim = NULL, gridy = TRUE, add.plot = FALSE, col = 1, ...)
}
\arguments{
  \item{x}{ data }
  \item{qdist}{ The probability function with which the data should be compared. }
  \item{probs}{ The selected probabilities, see details }
  \item{logx}{ if TRUE, then log scale on x-axis is used }
  \item{cex.lab}{ The size of the label }
  \item{xlab}{ title for x-axis}
  \item{ylab}{ title for y-axis }
  \item{line}{ if TRUE the line will be drawn}
  \item{lwd}{ the width of the line}
  \item{pch, cex, col}{ graphical parameter}
  \item{logfinetick}{ how fine are the tick marks on log-scale on x-axis }
  \item{logfinelab}{ how fine are the labels on log-scale on x-axis }
  \item{xlim}{ the range for the x-axis}
  \item{ylim}{ the range for the y-axis}
  \item{gridy}{ if grid along y-axis should be drawn }
  \item{add.plot}{ if TRUE the new plot is added to an old one }
  \item{\dots}{ futher arguments for the probability function}
}
\details{
First the probability of the sorted input x is computed and than the selected quantiles are calculated and after that plot is produced.
If probs=NULL then the 1\%, 5\%, 10\%, 20\%,...., 90\%, 95\% and 99\% quantile is taken.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\seealso{  \code{\link{plot}}, \code{\link{par}}, \code{\link{plot.default}} }
\examples{
data(AuNEW)
qpplot.das(AuNEW,qdist=qlnorm,xlab="Au",
ylab="Probabilities of lognormal distribution", pch=3,cex=0.7)
}
\keyword{dplot}
