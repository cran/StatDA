\name{boxplotlegend}
\alias{boxplotlegend}
\title{ Boxplotlegend }
\description{
  This function plots the legend in form of a boxplot. The symbols represent the different levels (e.g. whiskers, median, ...) of the boxplot.
}
\usage{
boxplotlegend(X, Y, el, boxinfo, x.shift = 40000, xf = 10000, y.shift = 0.2,
y.scale = 130000, legend.title = "Legend", cex.legtit = 1, logscale = TRUE,
symb = c(1, 1, 16, 3, 3), ssize = c(1.5, 1, 0.3, 1, 1.5), accentuate = FALSE,
cex.scale = 0.8)
}
\arguments{
  \item{X}{ X-coordinates }
  \item{Y}{ Y-coordinates }
  \item{el}{ variable considered }
  \item{boxinfo}{ from boxplot(el) or boxplotlog(el)  }
  \item{x.shift}{ shift in x-direction }
  \item{xf}{ width in x-direction }
  \item{y.shift}{ shift in y-direction (from title) }
  \item{y.scale}{ scale in y-direction }
  \item{legend.title}{ title for legend }
  \item{cex.legtit}{ cex of title for legend }
  \item{logscale}{ if TRUE plot boxplot in log-scale }
  \item{symb}{ symbols to be used (length 5!) }
  \item{ssize}{ symbol sizes to be used (length 5!) }
  \item{accentuate}{ if FALSE no symbols for the upper values (e.g. upper "hinge", upper whisker) are assigned }
  \item{cex.scale}{ cex for text "log-scale" for scale }
}
\details{
Takes the information provided by the argument boxinfo and plots a boxplot corresponding to the values. If there are no upper or/and lower outliers the symbols for the upper or/and lower whiskers will be ignored.
}
\value{
  Plots the legend with respect to the boxplot and returns the symbols, size and the quantiles used for the legend.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }
\examples{
#internal function, used in SymbLegend}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{aplot}