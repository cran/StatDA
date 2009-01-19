\encoding{latin1}
\name{moss}
\alias{moss}
\docType{data}
\title{Moss layer of the Kola Data}
\description{
The Kola Data were collected in the Kola Project (1993-1998,
Geological Surveys of Finland (GTK) and Norway (NGU) and Central
Kola Expedition (CKE), Russia). More than 600 samples in five
different layers were analysed, this dataset contains the C-horizon.
}
\usage{data(moss)}
\format{
  A data frame with 594 observations on the following 58 variables.
  \describe{
    \item{\code{ID}}{a numeric vector}
    \item{\code{XCOO}}{a numeric vector}
    \item{\code{YCOO}}{a numeric vector}
    \item{\code{ELEV}}{a numeric vector}
    \item{\code{COUN}}{a factor with levels \code{FIN} \code{NOR} \code{RUS}}
    \item{\code{ASP}}{a factor with levels \code{} \code{E} \code{FLAT} \code{N} \code{NE} \code{NW} \code{ NW} \code{S} \code{SE} \code{SW} \code{W}}
    \item{\code{GENLAN}}{a factor with levels \code{} \code{DEEPVAL} \code{FLA PLAIN} \code{FLAT} \code{HIMO} \code{LOWMO} \code{PLAIN} \code{PLAT} \code{RIDGE} \code{SLOPE}}
    \item{\code{TOPO}}{a factor with levels \code{} \code{BRUP} \code{BRUPLOW} \code{BRUPSTEE} \code{CONC} \code{CONCFLAT} \code{CONCLOW} \code{CONCMED} \code{CONCRUG} \code{CONCTERR} \code{CONV} \code{CONVLO} \code{CONVLOW} \code{CONVMED} \code{CONVTER} \code{FLAT} \code{FLATLOW} \code{FLATRUG} \code{FLATTER} \code{FLATTERR} \code{LOBRRUG} \code{LOW} \code{LOWBR} \code{LOWBRFLAT} \code{LOWBRLO} \code{LOWBRLOW} \code{LOWBRMED} \code{RUG} \code{RUGLOW} \code{TER} \code{TERLOW} \code{TERR} \code{TERRLOW} \code{TOHIFLAT} \code{TOP} \code{TOPFLAT} \code{TOPHILO} \code{TOPLOW} \code{TOPTER} \code{TOPUPBR} \code{UPBR} \code{UPBRFLAT} \code{UPBRLOW} \code{UPBRMED} \code{UPBRTER} \code{UPBRTERR} \code{UPTER}}
    \item{\code{GROUNDVEG}}{a factor with levels \code{} \code{BLUEBERRY} \code{CARLIN_HEATHER} \code{EMPETRUM} \code{GRASS} \code{LICHEN} \code{MOSS} \code{SHRUBS} \code{WHITE_LICHEN}}
    \item{\code{TREELAY}}{a factor with levels \code{} \code{BIPI} \code{BIPISPR} \code{BIRCH} \code{BIRCHdense} \code{BISPR} \code{BISPRPI} \code{MIX} \code{PIBI} \code{PIBISPR} \code{PINE} \code{PISPR} \code{PISPRBI} \code{SHRUBS} \code{SPARCEBI} \code{SPARCEPI} \code{SPRBI} \code{SPRBIPI} \code{SPRPI} \code{SPRPIBI} \code{SPRUCE} \code{WILLOW}}
    \item{\code{VEG_ZONE}}{a factor with levels \code{} \code{BOREAL_FOREST} \code{DWARF_SHRUB_TUNDRA} \code{FOREST_TUNDRA} \code{SHRUB_TUNDRA} \code{TUNDRA}}
    \item{\code{Date}}{a numeric vector}
    \item{\code{SAMP}}{a factor with levels \code{ALL} \code{ATMLRMA} \code{CRGKPCTF} \code{CRJHOJTV} \code{CRJHPC} \code{CRJHTF} \code{CROJTV} \code{CRPCTF} \code{CRPCTV} \code{CRTF} \code{DRMLRKK} \code{DRMRLKK} \code{GKJHOJ} \code{GKJHTV} \code{GKOJPCTV} \code{GKOJTF} \code{GKOJTV} \code{GKPCTF} \code{HARR} \code{JA} \code{JAMAMRL} \code{JAMLRMA} \code{JAMLRRR} \code{JARKP} \code{JARP} \code{JARPMA} \code{JARPMLR} \code{JARR} \code{JARRMLR} \code{JCPCTF} \code{JHGKTV} \code{JHOJGK} \code{JHOJTV} \code{JHPCTF} \code{JHRBTV} \code{Katanaev} \code{MAKKVG} \code{MARP} \code{MARPMLR} \code{MARPMRL} \code{MAVG} \code{MLR} \code{MLRJA} \code{MLRJARP} \code{MLRJARR} \code{MLRJSRR} \code{MLRMADR} \code{MLRMAJA} \code{MLRMARP} \code{MLRMAVG} \code{MLRMÖVG} \code{MLRRPJA} \code{MLRRPMA} \code{MRLMAJA} \code{OJGKTV} \code{OJTF} \code{Pavlov} \code{RPAV} \code{RPEM} \code{RPMA} \code{RPMLRJA} \code{RPMLRMA} \code{RPVM} \code{Semenov} \code{Smirnov} \code{TFOJ} \code{VGHNMA} \code{VGMA} \code{VGMAHN} \code{VGMARS} \code{VGMASR} \code{VGRSMA} \code{VMRP} \code{VMRPMA}}
    \item{\code{SPECIES}}{a factor with levels \code{} \code{HSDC} \code{HSDR} \code{HSRC} \code{HSSC} \code{HSSR} \code{PS} \code{PSDC} \code{PSDR} \code{PSRC} \code{PSRD} \code{PSSC} \code{PSSR} \code{SFDR}}
    \item{\code{LITO}}{a numeric vector}
    \item{\code{C_PAR}}{a factor with levels \code{} \code{BEDR} \code{FLUV} \code{FLUVG} \code{MAR} \code{SAP} \code{SEA} \code{STRAT} \code{TILL} \code{TILLSA} \code{TILLSAP} \code{TILL&SAP}}
    \item{\code{TOPC}}{a numeric vector}
    \item{\code{WEATH}}{a factor with levels \code{} \code{DRY} \code{DRY } \code{MIX} \code{MIX } \code{RAIN} \code{SNOW}}
    \item{\code{TEMP}}{a numeric vector}
    \item{\code{Ag}}{a numeric vector}
    \item{\code{Al}}{a numeric vector}
    \item{\code{As}}{a numeric vector}
    \item{\code{Au}}{a numeric vector}
    \item{\code{B}}{a numeric vector}
    \item{\code{Ba}}{a numeric vector}
    \item{\code{Be}}{a numeric vector}
    \item{\code{Bi}}{a numeric vector}
    \item{\code{Ca}}{a numeric vector}
    \item{\code{Cd}}{a numeric vector}
    \item{\code{Co}}{a numeric vector}
    \item{\code{Cr}}{a numeric vector}
    \item{\code{Cu}}{a numeric vector}
    \item{\code{Fe}}{a numeric vector}
    \item{\code{Hg}}{a numeric vector}
    \item{\code{K}}{a numeric vector}
    \item{\code{La}}{a numeric vector}
    \item{\code{Mg}}{a numeric vector}
    \item{\code{Mn}}{a numeric vector}
    \item{\code{Mo}}{a numeric vector}
    \item{\code{Na}}{a numeric vector}
    \item{\code{Ni}}{a numeric vector}
    \item{\code{P}}{a numeric vector}
    \item{\code{Pb}}{a numeric vector}
    \item{\code{Pd}}{a numeric vector}
    \item{\code{Pt}}{a numeric vector}
    \item{\code{Rb}}{a numeric vector}
    \item{\code{S}}{a numeric vector}
    \item{\code{Sb}}{a numeric vector}
    \item{\code{Sc}}{a numeric vector}
    \item{\code{Se}}{a numeric vector}
    \item{\code{Si}}{a numeric vector}
    \item{\code{Sr}}{a numeric vector}
    \item{\code{Th}}{a numeric vector}
    \item{\code{Tl}}{a numeric vector}
    \item{\code{U}}{a numeric vector}
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
\author{ Peter Filzmoser <\email{P.Filzmoser@tuwien.ac.at}>
\url{http://www.statistik.tuwien.ac.at/public/filz/}}
\examples{
data(moss)
str(moss)
}
\keyword{datasets}
