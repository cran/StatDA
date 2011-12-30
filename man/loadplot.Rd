\name{loadplot}
\alias{loadplot}
\title{ Plot the Loadings of a FA}
\description{
  Makes a Reimann-plot of a loadings matrix.
}
\usage{
loadplot(fa.object, titlepl = "Factor Analysis", crit = 0.3, length.varnames = 2)
}
\arguments{
  \item{fa.object}{ the output of factor analysis class }
  \item{titlepl}{ the title of the plot }
  \item{crit}{ all loadings smaller than crit will be ignored in the plot }
  \item{length.varnames}{ number of letters for abbreviating the variable names in the plot }
}
\value{
  Plot of the loadings of a FA therefore a object of factor analysis class must be provided.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
data(moss)
var=c("Ag","Al","As","B","Ba","Bi","Ca","Cd","Co","Cr","Cu","Fe","Hg","K","Mg","Mn","Mo",
      "Na","Ni","P","Pb","Rb","S","Sb","Si","Sr","Th","Tl","U","V","Zn")
x=log10(moss[,var])

x.mcd=covMcd(x,cor=TRUE)
x.rsc=scale(x,x.mcd$cent,sqrt(diag(x.mcd$cov)))
res5=pfa(x.rsc,factors=2,covmat=x.mcd,scores="regression",rotation="varimax",
    maxit=0,start=rep(0,ncol(x.rsc)))
loadplot(res5,titlepl="Robust FA (log-transformed)", crit=0.3)

}
\keyword{ dplot}
\keyword{ multivariate}
