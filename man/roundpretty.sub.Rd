\name{roundpretty.sub}
\alias{roundpretty.sub}
\title{ Subfunction for Roundpretty }
\description{
  This function rounds the number in pretty way. 
}
\usage{
roundpretty.sub(k, maxdig)
}
\arguments{
  \item{k}{ number to be rounded pretty }
  \item{maxdig}{ maximum number of digits after the coma }
}
\details{
  When maxdig is larger than 8 and the number is smaller than 0.00001, the number is rounded to 8 numbers after the coma. When the number ist smaller than 0.0001 the maximum numbers after the coma is 7, and so on.
}
\value{
 \item{kr}{rounded value}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }
\seealso{ \code{\link{roundpretty}} }
\keyword{ methods}
