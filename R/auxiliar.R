##
## Auxiliary functions for the geoR library
## ----------------------------------------
##
## These functions are typically called by other main functions
## to perform internal calculations
##


".check.borders" <-
  function(x)
{
  xname <- deparse(substitute(x))
  if(!inherits(x, "SpatialPolygons")) {
    if (is.matrix(x)) { 
      x <- x[,1:2]
    } else if(is.data.frame(x)) { 
      x <- as.matrix(x[,1:2])
    } else if(is.list(x)) {
      x <- matrix(unlist(x[1:2]), ncol=2) 
    } else stop(paste(xname, "must be a SpatialPolygons object or a matrix"))
    if(nrow(x) < 3) stop("borders must have at least 3 points")
    if(!identical(x[1,], x[nrow(x),])) x <- rbind(x, x[1,])
    x <- SpatialPolygons(list(Polygons(list(Polygon(coords=x)), ID="borders")))
  }
  return(x)
}


"polygrid" <- 
  function(xgrid, ygrid, borders, vec.inout = FALSE, ...)
{
  ## checking input
  if(!is.list(xgrid) && is.vector(drop(xgrid))){
    if(missing(ygrid))
      stop("xgrid must have x and y coordinates or a vector must be provided for ygrid")
    if(!is.vector(ygrid)) stop("ygrid must be a vector")
    xygrid <- expand.grid(x = xgrid, y = ygrid)
  }
  if(is.matrix(xgrid) || is.data.frame(xgrid)){
    if(ncol(xgrid) != 2)
      stop("xgrid must be a vector or a 2 column matrix or data-frame")
    xygrid <- xgrid
    if(!missing(ygrid)) warning("xgrid has 2 column, ygrid was ignored")
  }
  else
    if(is.list(xgrid)){
      if(length(xgrid) != 2)
        stop("if xgrid is a list it must have 2 elements")
      xygrid <- expand.grid(x = xgrid[[1]], y = xgrid[[2]])
      if(!missing(ygrid)) warning("xgrid is a list, ygrid was ignored")
    }
  borders <- .check.borders(borders)
#  if(nrow(borders) < 3) stop("borders must have at least 3 points")
#  if(!identical(borders[1,], borders[nrow(borders),]) 
#    borders <- rbind(borders, borders[1,])
  ind <- as.vector(.geoR_inout(pts=xygrid, poly=borders, ...))
  xypoly <- xygrid[ind == TRUE,  ]
  if(vec.inout == FALSE)
    return(xypoly)
  else return(list(xypoly = xypoly, vec.inout = ind))
}


".geoR_inout" <- function(pts, poly, ...) {
  ## inout returns logical vector
  ## poly <- .check.borders(poly)
  ## pts <- SpatialPoints(coords=pts)
  ## res <- over(pts, poly)
  ## !is.na(res)
  !is.na(sp::over(SpatialPoints(coords=pts), .check.borders(poly)))
}


