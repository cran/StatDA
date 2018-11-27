\encoding{latin1}
\name{plotbg}
\alias{plotbg}
\title{Kola background Plot}
\description{
Plots the Kola background
}
\usage{
plotbg(map = "kola.background", which.map = c(1, 2, 3, 4),
map.col = c(5, 1, 3, 4), map.lwd = c(2, 1, 2, 1), add.plot = FALSE, ...)
}
\arguments{
  \item{map}{List of coordinates. For the correct format see also help(kola.background)}
  \item{which.map}{which==1 ... plot project boundary \# which==2 ... plot coast line \# which==3 ... plot country borders \# which==4 ... plot lakes and rivers}
  \item{map.col}{Map colors to be used}
  \item{map.lwd}{Defines linestyle of the background}
  \item{add.plot}{logical. if true background is added to an existing plot}
  \item{\dots}{additional plot parameters, see help(par)}
}
\details{
Plots the background map of Kola
}
\references{
Reimann C, ?yr?s M, Chekushin V, Bogatyrev I, Boyd R, Caritat P de, Dutter R,
Finne TE, Halleraker JH, J?ger ?, Kashulina G, Lehto O, Niskavaara H, Pavlov V,
R?is?nen ML, Strand T, Volden T. Environmental Geochemical Atlas of the Central
Barents Region. NGU-GTK-CKE Special Publication, Geological Survey of Norway,
Trondheim, Norway, 1998.
}
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(kola.background)
plotbg()
}
\keyword{datasets}
