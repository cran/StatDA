\name{scatter3dPETER}
\alias{scatter3dPETER}
\title{ 3D plot of a Regression Model}
\description{
  This function makes a 3D plot of the data and the regression function. The user has the choice between different methods to calculate the coefficients for the regression model.
}
\usage{
scatter3dPETER(x, y, z, xlab = deparse(substitute(x)),
ylab = deparse(substitute(y)), zlab = deparse(substitute(z)),
revolutions = 0, bg.col = c("white", "black"),
axis.col = if (bg.col == "white") "black" else "white",
surface.col = c("blue", "green", "orange", "magenta", "cyan", "red",
"yellow", "gray"), neg.res.col = "red",
pos.res.col = "green", point.col = "yellow", text.col = axis.col,
grid.col = if (bg.col == "white") "black" else "gray",
fogtype = c("exp2", "linear", "exp", "none"),
residuals = (length(fit) == 1), surface = TRUE, grid = TRUE,
grid.lines = 26, df.smooth = NULL, df.additive = NULL, sphere.size = 1,
threshold = 0.01, speed = 1, fov = 60, fit = "linear", groups = NULL,
parallel = TRUE, model.summary = FALSE)
}
\arguments{
  \item{x, y, z}{ the coordinates for the points }
  \item{xlab, ylab, zlab}{ the labels for the axis }
  \item{revolutions}{ if the plot should be viewed from different angles }
  \item{bg.col, axis.col, surface.col, point.col, text.col, grid.col}{ define the colour for the background, axis,... }
  \item{pos.res.col, neg.res.col}{ colour for positive and negativ residuals}
  \item{fogtype}{ describes the fogtype, see rgl.bg }
  \item{residuals}{ if the residuals should be plotted }
  \item{surface}{ if the regression function should be plotted or just the points}
  \item{grid}{ if TRUE, the grid is plotted }
  \item{grid.lines}{ number of lines in the grid }
  \item{df.smooth}{ if fit=smooth, the number of degrees of freedom  }
  \item{df.additive}{ if fit=additive, the number of degrees of freedom }
  \item{sphere.size}{ a value for calibrating the size of the sphere }
  \item{threshold}{ the minimum size of the sphere, if the size is smaller than the threshold a point is plotted }
  \item{speed}{ if revolutions>0, how fast you make a 360 degree turn}
  \item{fov}{ field-of-view angle, see rgl.viewpoint }
  \item{fit}{ which method should be used for the model; "linear", "quadratic", "smooth" or "additive" }
  \item{groups}{ define groups for the points }
  \item{parallel}{ if groups is not NULL, a parallel shift in the model is made }
  \item{model.summary}{ if the summary should be returned }
}
\details{
The user can choose between a linear, quadratic, smoothed or additve model to calculate the coefficients.
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
#required library
#require(IPSUR)
data(chorizon)
lit=1
# This example needs additional libraries:
#scatter3dPETER(x=log10(chorizon[chorizon$LITO==lit,"Cr"]), 
#               z=log10(chorizon[chorizon$LITO==lit,"Cr_INAA"]),
#               y=log10(chorizon[chorizon$LITO==lit,"Co"]),
#               xlab="",ylab="",zlab="",
#               neg.res.col=gray(0.6), pos.res.col=gray(0.1), point.col=1, fov=30,
#               surface.col="black",grid.col="gray",sphere.size=0.8)
}
\keyword{ models}
\keyword{ dplot}
