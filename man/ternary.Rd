\name{ternary}
\alias{ternary}
\title{ Ternary plot}
\description{
  This plot shows the relative proportions of three variables in one diagramm. It is important that the proportion sum up to 100\% and if the values of the variables are very different it is important to scale them to the same data range.  
}
\usage{
ternary(x, nam = NULL, grid = FALSE, ...)
}
\arguments{
  \item{x}{ matrix with 3 columns }
  \item{nam}{ names of the variables }
  \item{grid}{ if TRUE the grid should be plotted }
  \item{\dots}{ further graphical parameters, see par }
}
\details{
The relative proportion of each variable is computed and those points are plotted into the graphic.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at> }
\examples{
data(moss)
x=moss[,c("Ni","Cu","Pb")]
ternary(x,grid=TRUE,pch=3,cex=0.7,col=1)
}
\keyword{ multivariate }