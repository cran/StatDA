\name{rg.remove.na}
\alias{rg.remove.na}
\title{ Remove NA }
\description{
  Function to remove NAs from a vector and inform the user of how many.
}
\usage{
rg.remove.na(xx)
}
\arguments{
  \item{xx}{ vector }
}
\details{
The function counts the NAs in a vector and returns the number of NAs and the "new" vector.
}
\value{
 \item{x}{vector without the NAs}
 \item{nna}{number of NAs removed}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }
\examples{
x<-rep(NA,10)
x[c(1,3,5,7,9)]<-10
rg.remove.na(x)
}
\keyword{ methods}
