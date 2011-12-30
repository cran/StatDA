\name{rg.mva}
\alias{rg.mva}
\title{ Non-robust Multivariate Data Analysis}
\description{
  Procedure to undertake non-robust multivariate data analysis. The saved list may be passed to other rotation and display functions
}
\usage{
rg.mva(x, main = deparse(substitute(x)))
}
\arguments{
  \item{x}{ data }
  \item{main}{ used for the list }
}
\details{
  Procesure to undertake non-robust multivariate data analyses; the object generated is identical to that of rg.robmva so that the savedlist may be passed to other rotation and display functions. Thus weights are set to 1, and other variables are set to appropriate defaults. The estimation of Mahalanobis distances is only undertaken if x is nonsingular, i.e. the lowest eigenvalue is > 10e-4.
}
\value{
  \item{n}{number of rows}
  \item{p}{number of columns}
  \item{wts}{the weights for the covariance matrix}
  \item{mean}{the mean of the data}
  \item{cov}{the covariance}
  \item{sd}{the standard deviation}
  \item{r}{correlation matrix}
  \item{eigenvalues}{eigenvalues of the SVD}
  \item{econtrib}{proportion of eigenvalues in \%}
  \item{eigenvectors}{eigenvectors of the SVD}
  \item{rload}{loadings matrix}
  \item{rcr}{standardised loadings matrix}
  \item{vcontrib}{scores variance}
  \item{pvcontrib}{proportion of scores variance in \%}
  \item{cpvcontrib}{cummulative proportion of scores variance}
  \item{md}{Mahalanbois distance}
  \item{ppm}{probability for outliegness using F-distribution}
  \item{epm}{probability for outliegness using Chisquared-distribution}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
#input data
data(ohorizon)
vegzn=ohorizon[,"VEG_ZONE"]
veg=rep(NA,nrow(ohorizon))
veg[vegzn=="BOREAL_FOREST"] <- 1
veg[vegzn=="FOREST_TUNDRA"] <- 2
veg[vegzn=="SHRUB_TUNDRA"] <- 3
veg[vegzn=="DWARF_SHRUB_TUNDRA"] <- 3
veg[vegzn=="TUNDRA"] <- 3
el=c("Ag","Al","As","B","Ba","Bi","Ca","Cd","Co","Cu","Fe","K","Mg","Mn",
  "Na","Ni","P","Pb","Rb","S","Sb","Sr","Th","Tl","V","Y","Zn")
x <- log10(ohorizon[!is.na(veg),el])
v <- veg[!is.na(veg)]

rg.mva(as.matrix(x[v==1,]))

}
\keyword{ multivariate}
