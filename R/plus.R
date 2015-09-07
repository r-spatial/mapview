if ( !isGeneric('+') ) {
  setGeneric('+', function(x, y, ...)
    standardGeneric('+'))
}

#' add a layer to a mapview or leaflet map
#'
#' @param e1 the map to which the layer should be added
#' @param e2 (spatial) object to be added
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
#'
#' ### point vector data ###
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#' m2 <- mapView(meuse)
#'
#' ### add two mapview objects
#' m1 + m2
#'
#' ### add spatial object to mapview object
#' m1 + meuse
#'
#' @name +
#' @docType methods
#' @rdname plus
#' @aliases +,mapview,mapview-method
NULL

setMethod("+",
          signature(e1 = "mapview",
                    e2 = "mapview"),
          function (e1, e2)
          {
            m <- e1@map
            m <- mapView(x = e2@object, map = m)
            ext <- extent(projectExtent(m@object, crs = llcrs))
            m <- leaflet::fitBounds(map = m@map,
                                    lng1 = ext@xmin,
                                    lat1 = ext@ymin,
                                    lng2 = ext@xmax,
                                    lat2 = ext@ymax)
            out <- new('mapview', object = e2@object, map = m)
            return(out)
          }
)

#' @name +
#' @docType methods
#' @rdname plus
#' @aliases +,mapview,ANY-method
#'
setMethod("+",
          signature(e1 = "mapview",
                    e2 = "ANY"),
          function (e1, e2)
          {
            m <- mapView(e2, map = e1@map)
            ext <- extent(projectExtent(m@object, crs = llcrs))
            m <- leaflet::fitBounds(map = m@map,
                                    lng1 = ext@xmin,
                                    lat1 = ext@ymin,
                                    lng2 = ext@xmax,
                                    lat2 = ext@ymax)
            out <- new('mapview', object = e2, map = m)
            return(out)
          }
)

#' @name +
#' @docType methods
#' @rdname plus
#' @aliases +,leaflet,ANY-method
#'
setMethod("+",
          signature(e1 = "leaflet",
                    e2 = "ANY"),
          function (e1, e2)
          {
            m <- mapView(e2, map = e1)
            ext <- extent(projectExtent(m@object, crs = llcrs))
            m <- leaflet::fitBounds(map = m@map,
                                    lng1 = ext@xmin,
                                    lat1 = ext@ymin,
                                    lng2 = ext@xmax,
                                    lat2 = ext@ymax)
            out <- new('mapview', object = e2, map = m)
            return(out)
          }
)
