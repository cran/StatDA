\name{roundpretty}
\alias{roundpretty}
\title{ Roundpretty}
\description{
  Round a value in a pretty way.
}
\usage{
roundpretty(kvec, maxdig)
}
\arguments{
  \item{kvec}{ the variable to be rounded }
  \item{maxdig}{ maximum number of digits after the coma }
}
\value{
 \item{result}{rounded value}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }
\seealso{ \code{\link{roundpretty.sub}} }
\examples{
roundpretty(0.873463029,5)
roundpretty(0.073463029,5)
roundpretty(0.003463029,5)
roundpretty(0.000463029,5)

}
\keyword{ methods}
