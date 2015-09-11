#' GIS-like viewing of spatial objects in R
#'
#' The package provides functionality to view spatial objects interactively
#' using leaflet.
#'
#' @name mapview-package
#' @aliases mapviewpackage
#' @docType package
#' @title GIS-like viewing of spatial objects in R
#' @author Tim Appelhans \cr
#' \cr
#' \emph{Maintainer:} Tim Appelhans \email{tim.appelhans@@gmail.com}
#'
#' @import leaflet sp raster satellite scales methods
#' @importFrom grDevices rgb
#'
#' @keywords package
#'
NULL
#'
#' @docType data
#' @name atlStorms2005
#' @title Atlantic Ocean storms 2005
#' @description Atlantic Ocean storms 2005
#' @details This dataset contains storm tracks for selected storms
#' in the Atlantic Ocean basin for the year 2005
#' @format \code{sp::SpatialLinesDataFrame}
NULL
#'
#' @docType data
#' @name gadmCHE
#' @title Administrative borders of Switzerland (level 2)
#' @description Administrative borders of Switzerland (level 2)
#' @details This dataset comes from \url{http://gadm.org}.
#' It was downloaded using \code{\link{getData}}.
#' @format \code{sp::SpatialPolygonsDataFrame}
#' @source
#' \url{http://gadm.org}
NULL
#'
#' @docType data
#' @name hillshadeCHE
#' @title Hillshade raster of Switzerland
#' @description Hillshade raster of Switzerland
#' @details This dataset was created following the example
#' at \code{\link{hillShade}}.
#' @format \code{"RasterLayer-class"}
NULL
#'
#' @docType data
#' @name tmeanCHE
#' @title Mean January temperature for Switzerland
#' @description Mean January temperature for Switzerland
#' @details This dataset was modified (resampled) from the example
#' at \code{\link{getData}}.
#' @format \code{"RasterLayer-class"}
#' @source
#' \url{http://worldclim.org}
NULL
#'
#' @docType data
#' @name kiliLS8
#' @title Landsat 8 detail of Mt Kilimanjaro
#' @description Landsat 8 detail of Mt Kilimanjaro showing an elevational
#' cross-section from savannah to alpine helichrysum vegetation
#' @details Use of this data requires your agreement to the USGS regulations on
#' using Landsat data.
#' @format \code{"RasterBrick-class"} with 11 bands.
#' @source
#' \url{http://earthexplorer.usgs.gov/}
NULL