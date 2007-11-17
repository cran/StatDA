\name{cor.sign}
\alias{cor.sign}
\title{ Correlation Matrix }
\description{
  Computes correlation matrix of x with method "pearson", "kendall" or "spearman". This function also prints the matrix with the significance levels.
}
\usage{
cor.sign(x, method = c("pearson", "kendall", "spearman"))
}
\arguments{
  \item{x}{ the data }
  \item{method}{ the method used }
}
\details{
This function estimate the association between paired samples an compute a test of the value being zero. All measures of association are in the range [-1,1] with 0 indicating no association.
}
\value{
  \item{cor}{Correlation matrix}
  \item{p.value}{p-value of the test statistic}
  }
\seealso{ \code{\link{cor.test}} }

\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }

\examples{
data(chorizon)
x=chorizon[,c("Ca","Cu","Mg","Na","P","Sr","Zn")]

cor.sign(log10(x),method="spearman")
}
\keyword{ univar}
\keyword{ multivariate }