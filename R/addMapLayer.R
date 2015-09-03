#' add a layer to a leaflet map using mapView
#'
#' @description
#' this function adds a layer to an interactive GIS-like view produced
#' with \code{\link{mapView}}
#'
#' @param object an object that can be passed to \code{\link{mapView}}
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
#' addMapLayer(meuse, m1)
#'
#' @export addMapLayer
#' @name addMapLayer
#' @rdname addMapLayer
#' @aliases addMapLayer

addMapLayer <- function(lay, map) {

  m <- mapView(x = lay, map = map)

  return(m)

}

# if ( !isGeneric("+") ) {
#   setGeneric("+", function(x, y, ...)
#     standardGeneric("+"))
# }
#

setMethod("+",
          signature(e1 = "mapview",
                    e2 = "mapview"),
          function (e1, e2)
          {
            addMapLayer(lay = e2@object, map = e1@map)
          }
)

setMethod("+",
          signature(e1 = "mapview",
                    e2 = "ANY"),
          function (e1, e2)
          {
            mapView(e2, map = e1@map)
          }
)
