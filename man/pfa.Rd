\name{pfa}
\alias{pfa}
\title{ Principal Factor Analysis }
\description{
  Computes the principal factor analysis of the input data.
}
\usage{
pfa(x, factors, data = NULL, covmat = NULL, n.obs = NA, subset, na.action,
start = NULL, scores = c("none", "regression", "Bartlett"),
rotation = "varimax", maxiter = 5, control = NULL, ...)
}
\arguments{
  \item{x}{ (robustly) scaled input data }
  \item{factors}{ number of factors }
  \item{data}{ default value is NULL }
  \item{covmat}{ (robustly) computed covariance or correlation matrix }
  \item{n.obs}{ number of observations }
  \item{subset}{ if a subset is used }
  \item{start}{ starting values }
  \item{scores}{ which method should be used to calculate the scores }
  \item{rotation}{ if a rotation should be made }
  \item{maxiter}{ maximum number of iterations  }
  \item{control}{ default value is NULL }
  \item{na.action}{what to do with NA values}
  \item{\dots}{arguments for creating a list}
}
\value{
 \item{loadings}{A matrix of loadings, one column for each factor. The factors are ordered in decreasing order of sums of squares of loadings.}
 \item{uniquness}{uniquness}
 \item{correlation}{correlation matrix}
 \item{criteria}{The results of the optimization: the value of the negativ log-likelihood and information of the iterations used.}
 \item{factors}{the factors}
 \item{dof}{degrees of freedom}
 \item{method}{"principal"}
 \item{n.obs}{number of observations if available, or NA}
 \item{call}{The matched call.}
 \item{STATISTIC, PVAL}{The significance-test statistic and p-value, if can be computed}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(moss)
var=c("Ni","Cu","Mg","Rb","Mn")
x=log10(moss[,var])

x.mcd=robustbase::covMcd(x,cor=TRUE)
x.rsc=scale(x,x.mcd$cent,sqrt(diag(x.mcd$cov)))
pfa(x.rsc,factors=2,covmat=x.mcd,scores="regression",rotation="varimax",
    maxit=0,start=rep(0,ncol(x.rsc)))

}
\keyword{ multivariate}
