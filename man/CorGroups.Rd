\name{CorGroups}
\alias{CorGroups}
\title{ Correlation Matrix for Sub-groups}
\description{
  The correlation matrix for sub-groups of data is computed and displayed in a graphic.
}
\usage{
CorGroups(dat, grouping, labels1, labels2, legend, ndigits = 4,
method = "pearson", ...)
}
\arguments{
  \item{dat}{ data values (probably log10-transformed)}
  \item{grouping}{ factor with levels for different groups }
  \item{labels1, labels2}{ labels for groups }
  \item{legend}{ plotting legend }
  \item{ndigits}{ number of digits to be used for plotting the numbers }
  \item{method}{ correlation method: "pearson", "spearman" or "kendall" }
  \item{\dots}{will not be used in the function}
}
\details{
The corralation is estimated with a non robust method but it is possible to select between the method of Pearson, Spearman and Kendall. The groups must be provided by the user.
}
\value{
  Graphic with the different sub-groups.
  }
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons Inc. To appear. }
\author{ Peter Filzmoser <P.Filzmoser@tuwien.ac.at>}
\examples{
data(chorizon)
x=chorizon[,c("Ca","Cu","Mg","Na","P","Sr","Zn")]

#definition of the groups
lit=chorizon[,"LITO"]
litolog=rep(NA, length(lit))
litolog[lit==10] <- 1
litolog[lit==52] <- 2
litolog[lit==81 | lit==82 | lit==83] <- 3
litolog[lit==7] <- 4
litolog <- litolog[!is.na(litolog)]
litolog <- factor(litolog, labels=c("AB","PG","AR","LPS"))

par(mfrow=c(1,1),mar=c(0.1,0.1,0.1,0.1))
CorGroups(log10(x), grouping=litolog, labels1=dimnames(x)[[2]],labels2=dimnames(x)[[2]],
legend=c("Caledonian Sediments","Basalts","Alkaline Rocks","Granites"),ndigits=2)

}
\keyword{ multivariate}
