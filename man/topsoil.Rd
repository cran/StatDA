\encoding{latin1}
\name{topsoil}
\alias{topsoil}
\docType{data}
\title{topsoil layer of the Kola Data}
\description{
The Kola Data were collected in the Kola Project (1993-1998,
Geological Surveys of Finland (GTK) and Norway (NGU) and Central
Kola Expedition (CKE), Russia). More than 600 samples in five
different layers were analysed, this dataset contains the C-horizon.
}
\usage{data(topsoil)}
\format{
  A data frame with 607 observations on the following 45 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{XCOO}}{a numeric vector}
    \item{\code{YCOO}}{a numeric vector}
    \item{\code{ELEV}}{a numeric vector}
    \item{\code{COUN}}{a factor with levels \code{} \code{FIN} \code{NOR} \code{RUS}}
    \item{\code{ASP}}{a factor with levels \code{} \code{E} \code{FLAT} \code{N} \code{NE} \code{NW} \code{ NW} \code{S} \code{SE} \code{SW} \code{W}}
    \item{\code{TOPC}}{a numeric vector}
    \item{\code{LITO}}{a numeric vector}
    \item{\code{Ac_228}}{a numeric vector}
    \item{\code{As}}{a numeric vector}
    \item{\code{Au}}{a numeric vector}
    \item{\code{Ba}}{a numeric vector}
    \item{\code{Bi_214}}{a numeric vector}
    \item{\code{Br}}{a numeric vector}
    \item{\code{Ca}}{a numeric vector}
    \item{\code{Ce}}{a numeric vector}
    \item{\code{Co}}{a numeric vector}
    \item{\code{Cr}}{a numeric vector}
    \item{\code{Cs}}{a numeric vector}
    \item{\code{Cs_137}}{a numeric vector}
    \item{\code{EC}}{a numeric vector}
    \item{\code{Eu}}{a numeric vector}
    \item{\code{Fe}}{a numeric vector}
    \item{\code{Hf}}{a numeric vector}
    \item{\code{Hg}}{a numeric vector}
    \item{\code{K_40}}{a numeric vector}
    \item{\code{La}}{a numeric vector}
    \item{\code{LOI}}{a numeric vector}
    \item{\code{Lu}}{a numeric vector}
    \item{\code{Mo}}{a numeric vector}
    \item{\code{Na}}{a numeric vector}
    \item{\code{Nd}}{a numeric vector}
    \item{\code{Ni}}{a numeric vector}
    \item{\code{pH}}{a numeric vector}
    \item{\code{Rb}}{a numeric vector}
    \item{\code{Sb}}{a numeric vector}
    \item{\code{Sc}}{a numeric vector}
    \item{\code{Sm}}{a numeric vector}
    \item{\code{Sr}}{a numeric vector}
    \item{\code{Tb}}{a numeric vector}
    \item{\code{Th}}{a numeric vector}
    \item{\code{U}}{a numeric vector}
    \item{\code{W}}{a numeric vector}
    \item{\code{Yb}}{a numeric vector}
    \item{\code{Zn}}{a numeric vector}
  }
}
\source{
Kola Project (1993-1998)
}
\references{
Reimann C, ?yr?s M, Chekushin V, Bogatyrev I, Boyd R, Caritat P de, Dutter R,
Finne TE, Halleraker JH, J?ger ?, Kashulina G, Lehto O, Niskavaara H, Pavlov V,
R?is?nen ML, Strand T, Volden T. Environmental Geochemical Atlas of the Central
Barents Region. NGU-GTK-CKE Special Publication, Geological Survey of Norway,
Trondheim, Norway, 1998.
}
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://cstat.tuwien.ac.at/filz/}}
\examples{
data(topsoil)
str(topsoil)
}
\keyword{datasets}
