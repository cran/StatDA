\name{monch}
\alias{monch}
\docType{data}
\title{ Boundary of the Monchegorsk area }
\description{
  This gives x- and y-coordinates with the boundary of the area around Monchegorsk.
}
\usage{data(monch)}
\format{
  The format is:
List of 2
 $ x: num [1:32] 710957 734664 754666 770223 779113 ...
 $ y: num [1:32] 7473981 7473143 7474818 7483191 7488215 ...
}
\details{
  This object can be used to select samples from the Kola data from the
  region around Monchegorsk.
}
\references{
C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear.}
\author{ P.Filzmoser <P.Filzmoser@tuwien.ac.at> }
\examples{
data(monch)
data(kola.background)
plotbg()
lines(monch$x,monch$y,col="red")
}
\keyword{datasets}
