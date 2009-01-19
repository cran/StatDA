\name{plotelement}
\alias{plotelement}
\title{ Plot Elements of a Discriminant Analysis }
\description{
  Plot the elements for the discriminant analysis. The plot is ordered in the different groups.
}
\usage{
plotelement(da.object)
}
\arguments{
  \item{da.object}{ a object of the lda class}
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
data(iris3)
Iris <- data.frame(rbind(iris3[,,1], iris3[,,2], iris3[,,3]), Sp = rep(c("s","c","v"), rep(50,3)))
train <- sample(1:150, 75) 
z <- lda(Sp ~ ., Iris, prior = c(1,1,1)/3, subset = train) 

plotelement(z)
}
\keyword{ dplot}
\keyword{ tree }
