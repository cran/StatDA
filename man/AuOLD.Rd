\name{AuOLD}
\alias{AuOLD}
\docType{data}
\title{ Au data, old }
\description{
  Au data from Kola C-horizon, old measurement method
}
\usage{data(AuOLD)}
\format{
  The format is:
 num [1:606] 0.001 0.001 0.002 0.001 0.007 0.006 0.001 0.001 0.001 0.001 ...
}
\details{
  These data of Au have much worse quality than the data AuNEW.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(AuNEW)
data(AuOLD)
plot(log10(AuOLD),log10(AuNEW))
}
\keyword{datasets}
