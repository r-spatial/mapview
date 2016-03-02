Proj4LeafletDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "proj4Leaflet",
      "0.7.2",
      system.file("htmlwidgets/lib/proj4Leaflet", package = "mapview"),
      script = "proj4.js",
      stylesheet = "proj4-compressed.css"
    )
  )
}

#' Add a measure control to the map.
#'
#' @param map a map widget object
#' @param position standard \href{http://leafletjs.com/reference.html#control-positions}{Leaflet control position options}.
#' @param primaryLengthUnit,secondaryLengthUnit units used to display length
#'           results. secondaryLengthUnit is optional.
#'           Valid values are \code{"feet"}, \code{"meters"}, \code{"miles"}, and \code{"kilometers"}.
#' @param primaryAreaUnit,secondaryAreaUnit units used to display area results.
#'           secondaryAreaUnit is optional.  Valid values are
#'           \code{"acres"}, \code{"hectares"}, \code{"sqmeters"}, and \code{"sqmiles"}.
#' @param activeColor base color to use for map features rendered while
#'           actively performing a measurement.
#'           Value should be a color represented as a hexadecimal string.
#' @param completedColor base color to use for features generated
#'           from a completed measurement.
#'           Value should be a color represented as a hexadecimal string.
#' @param popupOptions \code{list} of ptions applied to the popup
#'           of the resulting measure feature.
#'           Properties may be any \href{http://leafletjs.com/reference.html#popup-options}{standard Leaflet popup options}.
#'
#' @return modified map
#' @examples
#' library(mapview)
#'
#' leaf <- mapview()
#'   proj4Leaflet()
#'
#' leaf
#'
#' # customizing
#' leaf %>% addMeasure(
#'    position = "bottomleft"
#'   , primaryLengthUnit = "meters"
#'   , primaryAreaUnit = "sqmeters"
#'   , activeColor = "#3D535D"
#'   , completedColor = "#7D4479"
#' )
#'
#' @name addProj4Leaflet
#' @export addProj4Leaflet
addProj4Leaflet <- function() {
  map$dependencies <- c(map$dependencies, Proj4LeafletDependencies())
  invokeMethod(
    map
    , getMapData(map)
    , 'proj4Leaflet'
    , Filter(
      Negate(is.null)
      ,list(
        position = position
        , primaryLengthUnit = primaryLengthUnit
        , secondaryLengthUnit = secondaryLengthUnit
        , primaryAreaUnit = primaryAreaUnit
        , secondaryAreaUnit = secondaryAreaUnit
        , activeColor = activeColor
        , completedColor = completedColor
        , popupOptions = popupOptions
      )
    )
  )
}

