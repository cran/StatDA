\name{AuNEW}
\alias{AuNEW}
\docType{data}
\title{ Au data, new }
\description{
  Au data from Kola C-horizon, new measurement method
}
\usage{data(AuNEW)}
\format{
  The format is:
 num [1:606] 0.001344 0.000444 0.001607 0.000713 0.000898 ...
}
\details{
  These data of Au have much higher quality than the data AuOLD.
}
\references{
C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear.}
\author{ P.Filzmoser <P.Filzmoser@tuwien.ac.at> }
\examples{
data(AuNEW)
data(AuOLD)
plot(log10(AuOLD),log10(AuNEW))
}
\keyword{datasets}
