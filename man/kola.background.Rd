\encoding{latin1}
\name{kola.background}
\alias{kola.background}
\title{kola.background}
\description{
Coordinates of the Kola background. Seperate polygons for the project boundary, borders, lakes and coast are provided.
}
\usage{data(kola.background)}
\format{
  The format is:
List of 4
 $ boundary:`data.frame':   50 obs. of  2 variables:
  ..$ V1: num [1:50] 388650 388160 386587 384035 383029 ...
  ..$ V2: num [1:50] 7892400 7881248 7847303 7790797 7769214 ...
 $ coast   :`data.frame':   6259 obs. of  2 variables:
  ..$ V1: num [1:6259] 438431 439102 439102 439643 439643 ...
  ..$ V2: num [1:6259] 7895619 7896495 7896495 7895800 7895542 ...
 $ borders :`data.frame':   504 obs. of  2 variables:
  ..$ V1: num [1:504] 417575 417704 418890 420308 422731 ...
  ..$ V2: num [1:504] 7612984 7612984 7613293 7614530 7615972 ...
 $ lakes   :`data.frame':   6003 obs. of  2 variables:
  ..$ V1: num [1:6003] 547972 546915     NA 547972 547172 ...
  ..$ V2: num [1:6003] 7815109 7815599      NA 7815109 7813873 ...
}
\details{
Is used by plotbg()
}
\source{
Kola Project (1993-1998)
}
\references{
Reimann C, Ayras M, Chekushin V, Bogatyrev I, Boyd R, Caritat P de, Dutter R,
Finne TE, Halleraker JH, Jager O, Kashulina G, Lehto O, Niskavaara H, Pavlov V,
Raisanen ML, Strand T, Volden T. Environmental Geochemical Atlas of the Central
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
