\name{factanal.fit.principal}
\alias{factanal.fit.principal}
\title{ Fit a Factor Analysis }
\description{
  Internal function for pfa.
}
\usage{
factanal.fit.principal(cmat, factors, p = ncol(cmat), start = NULL,
iter.max = 10, unique.tol = 1e-04)
}
\arguments{
  \item{cmat}{ provided correlation matrix}
  \item{factors}{ number of factors }
  \item{p}{ number of observations }
  \item{start}{ vector of start values }
  \item{iter.max}{ maximum number of iteration used to calculate the common factor }
  \item{unique.tol}{ the tolerance for a deviation of the maximum (in each row, without the diag) value of the given correlation matrix to the new calculated value }
}
\value{
 \item{loadings}{A matrix of loadings, one column for each factor. The factors are ordered in decreasing order of sums of squares of loadings.}
 \item{uniquness}{uniquness}
 \item{correlation}{correlation matrix}
 \item{criteria}{The results of the optimization: the value of the negativ log-likelihood and information of the iterations used.}
 \item{factors}{the factors}
 \item{dof}{degrees of freedom}
 \item{method}{"principal"}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }
\keyword{multivariate}
