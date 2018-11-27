\name{bordersKola}
\alias{bordersKola}
\docType{data}
\title{ Borders of the Kola Project boundary }
\description{
x- and y-coordinates of the Kola Project boundary.
}
\usage{data(bordersKola)}
\format{
  The format is:
List of 2
 $ x: num [1:64] 836200 881000 752900 743100 737500 ...
 $ y: num [1:64] 7708867 7403003 7389239 7377769 7364006 ...
}
\details{
The corrdinates for the Kola Project boundary are used for the surface
maps, i.e. for Krige and Smoothing maps. It is a list with two list
elements x and y for the x- and y-coordinates.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(bordersKola)
plot(bordersKola$x,bordersKola$y)
}
\keyword{datasets}
