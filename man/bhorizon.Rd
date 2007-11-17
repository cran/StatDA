\encoding{latin1}
\name{bhorizon}
\alias{bhorizon}
\docType{data}
\title{B-horizon of the Kola Data}
\description{
The Kola Data were collected in the Kola Project (1993-1998,
Geological Surveys of Finland (GTK) and Norway (NGU) and Central
Kola Expedition (CKE), Russia). More than 600 samples in five
different layers were analysed, this dataset contains the C-horizon.
}
\usage{data(bhorizon)}
\format{
  A data frame with 609 observations on the following 77 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{XCOO}}{a numeric vector}
    \item{\code{YCOO}}{a numeric vector}
    \item{\code{ELEV}}{a numeric vector}
    \item{\code{COUN}}{a factor with levels \code{FIN} \code{NOR} \code{RUS}}
    \item{\code{ASP}}{a factor with levels \code{} \code{E} \code{FLAT} \code{N} \code{NE} \code{NW} \code{ NW} \code{S} \code{SE} \code{SW} \code{W}}
    \item{\code{LOWDB}}{a numeric vector}
    \item{\code{LITO}}{a numeric vector}
    \item{\code{GENLAN}}{a factor with levels \code{} \code{DEEPVAL} \code{FLA PLAIN} \code{FLAT} \code{HIMO} \code{LOWMO} \code{PLAIN} \code{PLAT} \code{RIDGE} \code{SLOPE}}
    \item{\code{Ag}}{a numeric vector}
    \item{\code{Al}}{a numeric vector}
    \item{\code{Al_XRF}}{a numeric vector}
    \item{\code{Al2O3}}{a numeric vector}
    \item{\code{As}}{a numeric vector}
    \item{\code{Au}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{Ba}}{a numeric vector}
    \item{\code{Be}}{a numeric vector}
    \item{\code{Bi}}{a numeric vector}
    \item{\code{Br_IC}}{a numeric vector}
    \item{\code{Ca}}{a numeric vector}
    \item{\code{Ca_XRF}}{a numeric vector}
    \item{\code{CaO}}{a numeric vector}
    \item{\code{Cd}}{a numeric vector}
    \item{\code{Cl_IC}}{a numeric vector}
    \item{\code{Co}}{a numeric vector}
    \item{\code{Cr}}{a numeric vector}
    \item{\code{Cu}}{a numeric vector}
    \item{\code{EC}}{a numeric vector}
    \item{\code{F_IC}}{a numeric vector}
    \item{\code{Fe}}{a numeric vector}
    \item{\code{Fe_XRF}}{a numeric vector}
    \item{\code{Fe2O3}}{a numeric vector}
    \item{\code{Hg}}{a numeric vector}
    \item{\code{K}}{a numeric vector}
    \item{\code{K_XRF}}{a numeric vector}
    \item{\code{K2O}}{a numeric vector}
    \item{\code{La}}{a numeric vector}
    \item{\code{Li}}{a numeric vector}
    \item{\code{LOI}}{a numeric vector}
    \item{\code{Mg}}{a numeric vector}
    \item{\code{Mg_XRF}}{a numeric vector}
    \item{\code{MgO}}{a numeric vector}
    \item{\code{Mn}}{a numeric vector}
    \item{\code{Mn_XRF}}{a numeric vector}
    \item{\code{MnO}}{a numeric vector}
    \item{\code{Mo}}{a numeric vector}
    \item{\code{Na}}{a numeric vector}
    \item{\code{Na_XRF}}{a numeric vector}
    \item{\code{Na2O}}{a numeric vector}
    \item{\code{Ni}}{a numeric vector}
    \item{\code{NO3_IC}}{a numeric vector}
    \item{\code{P}}{a numeric vector}
    \item{\code{P_XRF}}{a numeric vector}
    \item{\code{P2O5}}{a numeric vector}
    \item{\code{Pb}}{a numeric vector}
    \item{\code{Pd}}{a numeric vector}
    \item{\code{pH}}{a numeric vector}
    \item{\code{PO4_IC}}{a numeric vector}
    \item{\code{Pt}}{a numeric vector}
    \item{\code{S}}{a numeric vector}
    \item{\code{Sb}}{a numeric vector}
    \item{\code{Sc}}{a numeric vector}
    \item{\code{Se}}{a numeric vector}
    \item{\code{Si}}{a numeric vector}
    \item{\code{Si_XRF}}{a numeric vector}
    \item{\code{SiO2}}{a numeric vector}
    \item{\code{SO4_IC}}{a numeric vector}
    \item{\code{Sr}}{a numeric vector}
    \item{\code{Te}}{a numeric vector}
    \item{\code{Th}}{a numeric vector}
    \item{\code{Ti}}{a numeric vector}
    \item{\code{Ti_XRF}}{a numeric vector}
    \item{\code{TiO2}}{a numeric vector}
    \item{\code{V}}{a numeric vector}
    \item{\code{Y}}{a numeric vector}
    \item{\code{Zn}}{a numeric vector}
  }
}
\source{
Kola Project (1993-1998)
}
\references{
Reimann C, Äyräs M, Chekushin V, Bogatyrev I, Boyd R, Caritat P de, Dutter R,
Finne TE, Halleraker JH, Jæger Ø, Kashulina G, Lehto O, Niskavaara H, Pavlov V,
Räisänen ML, Strand T, Volden T. Environmental Geochemical Atlas of the Central
Barents Region. NGU-GTK-CKE Special Publication, Geological Survey of Norway,
Trondheim, Norway, 1998.
}
\examples{
data(bhorizon)
str(bhorizon)
}
\keyword{datasets}
