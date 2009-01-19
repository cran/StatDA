\name{Northarrow}
\alias{Northarrow}
\title{ Northarrow}
\description{
  Add a North Arrow to a map.
}
\usage{
Northarrow(Xbottom, Ybottom, Xtop, Ytop, Xtext, Ytext, Alength, Aangle, Alwd,
Tcex)
}
\arguments{
  \item{Xbottom}{ x coordinate of the first point }
  \item{Ybottom}{ y coordinate of the first point }
  \item{Xtop}{ x coordinate of the second point }
  \item{Ytop}{ y coordinate of the second point }
  \item{Xtext}{ x coordinate of the label }
  \item{Ytext}{ y coordinate of the label }
  \item{Alength}{ length of the edges of the arrow head (in inches) }
  \item{Aangle}{ angle from the shaft of the arrow to the edge of the arrow head }
  \item{Alwd}{ The line width, a positive number }
  \item{Tcex}{ numeric character expansion factor }
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
plot.new()
Northarrow(0.5,0,0.5,1,0.5,0.5,Alength=0.15,Aangle=15,Alwd=2,Tcex=2)
}
\keyword{ aplot}
