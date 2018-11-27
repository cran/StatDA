\name{branch-class}
\docType{class}
\alias{branch-class}
\alias{plot,branch,ANY-method}
\alias{show,branch-method}

\title{Class "branch" }
\description{ A Composite object used to plot trees as multivariate graphics }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("branch", ...)}.
}
\section{Slots}{
     \describe{
    \item{\code{LR}:}{Object of class \code{"numeric"}  }
    \item{\code{w}:}{Object of class \code{"numeric"}  }
    \item{\code{h}:}{Object of class \code{"numeric"}  }
    \item{\code{El}:}{Object of class \code{"numeric"}  }
    \item{\code{LeafL}:}{Object of class \code{"branch"}  }
    \item{\code{LeafR}:}{Object of class \code{"branch"}  }
    \item{\code{Bole}:}{Object of class \code{"numeric"}  }
  }
}
\section{Extends}{
Class \code{"\linkS4class{Component}"}, directly.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "branch", y = "ANY")}:  }
    \item{show}{\code{signature(object = "branch")}:  }
     }
}
\references{ C. Reimann, P. Filzmoser, R.G. Garrett, and R. Dutter: Statistical Data Analysis
   Explained. Applied Environmental Statistics with R. John Wiley and Sons, Chichester, 2008. }
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
    showClass("branch")
}
\keyword{classes}
