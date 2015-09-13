#' add a layer to a leaflet map using mapView
#'
#' @description
#' this function adds a layer to an interactive GIS-like view produced
#' with \code{\link{mapView}}
#'
#' @param lay an object that can be passed to \code{\link{mapView}}
#' @param map the map to which the layer should be added
#' @param ... additional arguments passed on to \code{\link{mapView}}
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' ### raster data ###
#' library(sp)
#' library(raster)
#'
#' data(meuse.grid)
#' coordinates(meuse.grid) = ~x+y
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' gridded(meuse.grid) = TRUE
#' meuse_rst <- stack(meuse.grid)
#'
#' m1 <- mapView(meuse_rst[[3]])
#' m1
#'
#' ### point vector data ###
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#' # all layers of meuse
#' addMapLayer(meuse, slot(m1, "map"))
#'
#' @export addMapLayer
#' @name addMapLayer
#' @rdname addMapLayer
#' @aliases addMapLayer
NULL

addMapLayer <- function(lay, map) {

  if (class(map)[1] == "mapview") map <- map@map
  m <- mapView(x = lay, map = map)

  return(m)

}

# if ( !isGeneric("+") ) {
#   setGeneric("+", function(x, y, ...)
#     standardGeneric("+"))
# }
#


