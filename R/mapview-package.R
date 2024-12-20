#' @import leaflet sp sf raster satellite scales methods png lattice base64enc
#' @importFrom grDevices rgb dev.off png svg colorRampPalette grey.colors
#' @importFrom utils find glob2rx
#' @importFrom utils modifyList
#'
#' @details
#' The package provides functionality to view spatial objects interactively.
#' The intention is to provide interactivity for easy and quick visualization
#' during spatial data analysis. It is not intended for fine-tuned presentation
#' quality map production.
#'
"_PACKAGE"

#' Selected breweries in Franconia
#' @details This dataset contains selected breweries in Franconia. It is partly a
#' subset of a larger database that was compiled by students at the
#' University of Marburg for a seminar called
#' "The Geography of Beer: sustainability in the food industry"
#' and partly consists of breweries downloaded from
#' \url{https://www.bierwandern.de/inhalt/brauereiliste.html} with the kind permission
#' of Rainer Kastl. Note that use of these data is restricted to  non-commercial use
#' and that they are explicitly excluded from the GPL license that mapview is licensed under.
#' @format \code{sf feature collection POINT}
"breweries"

#' Administrative district borders of Franconia
#'
#' @details The NUTS_2013_01M_SH.zip archive was downloaded on 23/03/2017 from
#' \url{https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts}.
#' \url{https://gist.github.com/tim-salabim/2845fa90813fa25c18cf83f9b88cbde0}
#' @format \code{sf feature collection MULTIPOLYGON}
#' @source
#' \url{https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts}
"franconia"

#' Selected hiking trails in Franconia
#'
#' @details These hiking trails were downloaded on 06/04/2017 from
#' \url{https://geoportal.bayern.de/bayernatlas}
#' These data are published by the owner under Creative Commons Namensnennung 3.0 Deutschland,
#' see \url{https://creativecommons.org/licenses/by/3.0/de/} for details.
#' @format \code{sf feature collection MULTILINESTRING}
#' @source
#' Datenquelle: Bayerische Vermessungsverwaltung - `www.geodaten.bayern.de`
#' \url{https://www.ldbv.bayern.de/produkte/weitere/opendata.html}
"trails"

