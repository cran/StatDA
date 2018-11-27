\name{rg.wtdsums}
\alias{rg.wtdsums}
\title{ Calculate Weighted Sums for a Matrix }
\description{
  This function computes a weighted sum for a matrix based on computed quantiles and user defined relative importance.
}
\usage{
rg.wtdsums(x, ri, xcentr = NULL, xdisp = NULL)
}
\arguments{
  \item{x}{ matrix }
  \item{ri}{ vector for the relative importance, length(ri)=length(x[1,]) }
  \item{xcentr}{ the provided center }
  \item{xdisp}{ the provided variance }
}
\details{
  It is not necessary to provide the center and the variance. If those values are not supplied the center is the 50\% quantile and the variance is calculated from the 25\% and 75\% quantile.
}
\value{
  \item{input}{input parameter}
  \item{centr}{the center}
  \item{disp}{the variance}
  \item{ri}{relative importance}
  \item{w}{weights}
  \item{a}{normalized weights}
  \item{ws}{normalized weights times standardized x}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(chorizon)
var=c("Si_XRF","Al_XRF","K_XRF","LOI","P","Mn")
ri=c(-2.0,1.5,2.0,2.0,3.0,2.0)
x=chorizon[,var]
rg.wtdsums(x,ri)
}
\keyword{ multivariate}
