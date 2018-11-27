\name{varcomp}
\alias{varcomp}
\title{ Variance Components}
\description{
  This function estimates the variance components for ANOVA.
}
\usage{
varcomp(a1, a2, f1, f2)
}
\arguments{
  \item{a1, a2}{ analytical duplicates }
  \item{f1, f2}{ field duplicates }
}
\value{
 \item{pct.regional}{percentage of regional variability}
 \item{pct.site}{percentage at site variability}
 \item{pct.analytical}{percentage of analytical variability}
 \item{pval}{p-value}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
# field duplicates:
data(CHorFieldDUP)
xfield1=CHorFieldDUP[,5:98]
xfield2=CHorFieldDUP[,99:192]

# anaytical duplicates:
data(CHorANADUP)
xanal1=CHorANADUP[,3:96]
xanal2=CHorANADUP[,97:190]

varcomp(xanal1[,1],xanal2[,1],xfield1[,1],xfield2[,1])
}
\keyword{ univar}
