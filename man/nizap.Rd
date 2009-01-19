\name{nizap}
\alias{nizap}
\docType{data}
\title{ Boundary of the area Nikel-Zapoljarnij }
\description{
  This gives x- and y-coordinates with the boundary of the area around Nikel-Zapoljarnij.
}
\usage{data(nizap)}
\format{
  The format is:
List of 2
 $ x: num [1:36] 699104 693918 681324 662062 645023 ...
 $ y: num [1:36] 7739416 7746115 7751139 7756163 7757000 ...
}
\details{
  This object can be used to select samples from the Kola data from the
  region around Nikel-Zapoljarnij.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
data(nizap)
data(kola.background)
plotbg()
lines(nizap$x,nizap$y,col="red")
}
\keyword{datasets}
