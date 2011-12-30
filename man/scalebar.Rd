\name{scalebar}
\alias{scalebar}
\title{ Scalebar }
\description{
  This function plots the unit at a specified location.
}
\usage{
scalebar(Xlowerleft, Ylowerleft, Xupperright, Yupperright, shifttext, shiftkm,
sizetext)
}
\arguments{
  \item{Xlowerleft, Ylowerleft}{ x and y coordinate of the lower left corner}
  \item{Xupperright, Yupperright}{ x and y coordinate of the upper corner }
  \item{shifttext}{ on which margin line, starting at 0 counting outwards }
  \item{shiftkm}{ how far from the last point the label should be written }
  \item{sizetext}{ expansion factor for the text }
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
plot.new()
scalebar(0,0.25,1,0.5,shifttext=-0.05,shiftkm=4e4,sizetext=0.8)
}
\keyword{ aplot}
