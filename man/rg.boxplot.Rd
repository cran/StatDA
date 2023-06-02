\name{rg.boxplot}
\alias{rg.boxplot}
\title{ Plot a Boxplot }
\description{
  Plot a single horizontal boxplot, the default is a Tukey boxplot.
}
\usage{
rg.boxplot(xx, xlab = deparse(substitute(xx)), log = FALSE, ifbw = FALSE,
wend = 0.05, xlim = NULL, main = " ", colr = 5, ...)
}
\arguments{
  \item{xx}{ data }
  \item{xlab}{ label for the x-axis }
  \item{log}{ if TRUE, a log-scaled plot and a logtransformation of the data }
  \item{ifbw}{ if TRUE, a IDEAS style box-and-whisker plot is produced }
  \item{wend}{ defines the end of the whisker, default is 5\% and 95\% quantile }
  \item{xlim}{ setting xlim results in outliers not being plotted as the x-axis is shortened.}
  \item{main}{ main title of the plot }
  \item{colr}{ the box is infilled with a yellow ochre; if no colour is required set colr=0 }
  \item{\dots}{ further graphical parameters for the plot}
}
\value{
  No return value, creates a plot.
}
\details{
As the x-axis is shortend by setting xlim, however, the statistics used to define the boxplot, or box-and-whisker plot, are still based on the total data set. To plot a truncated data set create a subset first, or use the x[x<some.value] construct in the call.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(chorizon)
Ba=chorizon[,"Ba"]
rg.boxplot(Ba,ifbw=TRUE,colr=0,xlab="Ba [mg/kg]",cex.lab=1.2)
}
\keyword{ dplot }
\keyword{ univar }
