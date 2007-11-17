\encoding{latin1}
\name{ohorizon}
\alias{ohorizon}
\docType{data}
\title{O-horizon of the Kola Data}
\description{
The Kola Data were collected in the Kola Project (1993-1998,
Geological Surveys of Finland (GTK) and Norway (NGU) and Central
Kola Expedition (CKE), Russia). More than 600 samples in five
different layers were analysed, this dataset contains the O-horizon.
}
\usage{data(ohorizon)}
\format{
  A data frame with 617 observations on the following 85 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{XCOO}}{a numeric vector}
    \item{\code{YCOO}}{a numeric vector}
    \item{\code{ELEV}}{a numeric vector}
    \item{\code{COUN}}{a factor with levels \code{FIN} \code{NOR} \code{RUS}}
    \item{\code{X.ASP}}{a factor with levels \code{-9999} \code{E} \code{FLAT} \code{N} \code{NE} \code{NW} \code{ NW} \code{S} \code{SE} \code{SW} \code{W}}
    \item{\code{AoMEAN}}{a numeric vector}
    \item{\code{HUMNO}}{a numeric vector}
    \item{\code{HUMTHI}}{a numeric vector}
    \item{\code{GROUNDVEG}}{a factor with levels \code{} \code{BLUEBERRY} \code{CARLIN_HEATHER} \code{EMPETRUM} \code{GRASS} \code{LICHEN} \code{MOSS} \code{SHRUBS} \code{WHITE_LICHEN}}
    \item{\code{TREELAY}}{a factor with levels \code{} \code{BIPI} \code{BIPISPR} \code{BIRCH} \code{BIRCHdense} \code{BISPR} \code{BISPRPI} \code{MIX} \code{PIBI} \code{PIBISPR} \code{PINE} \code{PISPR} \code{PISPRBI} \code{SHRUBS} \code{SPARCEBI} \code{SPARCEPI} \code{SPRBI} \code{SPRBIPI} \code{SPRPI} \code{SPRPIBI} \code{SPRUCE} \code{WILLOW}}
    \item{\code{VEG_ZONE}}{a factor with levels \code{} \code{BOREAL_FOREST} \code{DWARF_SHRUB_TUNDRA} \code{FOREST_TUNDRA} \code{SHRUB_TUNDRA} \code{TUNDRA}}
    \item{\code{LITO}}{a numeric vector}
    \item{\code{Ag}}{a numeric vector}
    \item{\code{Al}}{a numeric vector}
    \item{\code{Al_AA}}{a numeric vector}
    \item{\code{As}}{a numeric vector}
    \item{\code{Au}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{Ba}}{a numeric vector}
    \item{\code{Ba_AA}}{a numeric vector}
    \item{\code{Be}}{a numeric vector}
    \item{\code{Bi}}{a numeric vector}
    \item{\code{Br}}{a numeric vector}
    \item{\code{C}}{a numeric vector}
    \item{\code{Ca}}{a numeric vector}
    \item{\code{Ca_AA}}{a numeric vector}
    \item{\code{Cd}}{a numeric vector}
    \item{\code{Cd_AA}}{a numeric vector}
    \item{\code{Cl}}{a numeric vector}
    \item{\code{Co}}{a numeric vector}
    \item{\code{Co_AA}}{a numeric vector}
    \item{\code{Cond}}{a numeric vector}
    \item{\code{Cr}}{a numeric vector}
    \item{\code{Cr_AA}}{a numeric vector}
    \item{\code{Cu}}{a numeric vector}
    \item{\code{Cu_AA}}{a numeric vector}
    \item{\code{F}}{a numeric vector}
    \item{\code{Fe}}{a numeric vector}
    \item{\code{Fe_AA}}{a numeric vector}
    \item{\code{H}}{a numeric vector}
    \item{\code{Hg}}{a numeric vector}
    \item{\code{K}}{a numeric vector}
    \item{\code{K_AA}}{a numeric vector}
    \item{\code{La}}{a numeric vector}
    \item{\code{LOI}}{a numeric vector}
    \item{\code{Mg}}{a numeric vector}
    \item{\code{Mg_AA}}{a numeric vector}
    \item{\code{Mn}}{a numeric vector}
    \item{\code{Mn_AA}}{a numeric vector}
    \item{\code{Mo}}{a numeric vector}
    \item{\code{N}}{a numeric vector}
    \item{\code{Na}}{a numeric vector}
    \item{\code{Na_AA}}{a numeric vector}
    \item{\code{Ni}}{a numeric vector}
    \item{\code{Ni_AA}}{a numeric vector}
    \item{\code{NO3}}{a numeric vector}
    \item{\code{P}}{a numeric vector}
    \item{\code{P_AA}}{a numeric vector}
    \item{\code{Pb}}{a numeric vector}
    \item{\code{Pb_AA}}{a numeric vector}
    \item{\code{Pd}}{a numeric vector}
    \item{\code{pH}}{a numeric vector}
    \item{\code{PO4}}{a numeric vector}
    \item{\code{Pt}}{a numeric vector}
    \item{\code{Rb}}{a numeric vector}
    \item{\code{S}}{a numeric vector}
    \item{\code{S_AA}}{a numeric vector}
    \item{\code{Sb}}{a numeric vector}
    \item{\code{Sc}}{a numeric vector}
    \item{\code{Se}}{a numeric vector}
    \item{\code{Si}}{a numeric vector}
    \item{\code{Si_AA}}{a numeric vector}
    \item{\code{SO4}}{a numeric vector}
    \item{\code{Sr}}{a numeric vector}
    \item{\code{Sr_AA}}{a numeric vector}
    \item{\code{Th}}{a numeric vector}
    \item{\code{Ti_AA}}{a numeric vector}
    \item{\code{Tl}}{a numeric vector}
    \item{\code{U}}{a numeric vector}
    \item{\code{V}}{a numeric vector}
    \item{\code{V_AA}}{a numeric vector}
    \item{\code{Y}}{a numeric vector}
    \item{\code{Zn}}{a numeric vector}
    \item{\code{Zn_AA}}{a numeric vector}
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
data(ohorizon)
str(ohorizon)
}
\keyword{datasets}
