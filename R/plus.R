if ( !isGeneric('+') ) {
  setGeneric('+', function(x, y, ...)
    standardGeneric('+'))
}

#' Add a layer to a mapview or leaflet map
#'
#' @param e1 the map to which the layer should be added
#' @param e2 (spatial) object to be added
#'
#' @examples
#' ### raster data ###
#' library(sp)
#' library(raster)
#'
#' m1 <- mapView(poppendorf[[10]])
#'
#' ### point vector data ###
#' m2 <- mapView(breweries91)
#'
#' ### add two mapview objects
#' m1 + m2 # final zoom level based on m2
#' '+'(m2, m1) # final zoom level based on m1
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
            is.ext <- class(e2@object[[length(e2@object)]]) == "Extent"
            if (is.ext) {
              rst <- raster::raster(e2@object[[length(e2@object)]])
              raster::projection(rst) <- llcrs
            }
            m <- e1@map
            m <- appendMapCallEntries(m, e2@map)
            out_obj <- append(e1@object, e2@object)
            if (length(e2@object[[length(e2@object)]]) > 1) {
              if (is.ext) ext <- raster::extent(rst) else
                ext <- raster::extent(
                  raster::projectExtent(out_obj[[length(out_obj)]],
                                        crs = llcrs))
              m <- leaflet::fitBounds(map = m,
                                      lng1 = ext@xmin,
                                      lat1 = ext@ymin,
                                      lng2 = ext@xmax,
                                      lat2 = ext@ymax)
              #m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
            }

            out <- methods::new('mapview', object = out_obj, map = m)
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
            nm <- deparse(substitute(e2))
            m <- mapView(e2, map = e1@map, layer.name = nm)
            out_obj <- append(e1@object, m@object)

            if (length(e2) > 1) {
              ext <- raster::extent(
                raster::projectExtent(out_obj[[length(out_obj)]],
                                      crs = llcrs))
              m <- leaflet::fitBounds(map = m@map,
                                      lng1 = ext@xmin,
                                      lat1 = ext@ymin,
                                      lng2 = ext@xmax,
                                      lat2 = ext@ymax)
              #m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
            }

            out <- methods::new('mapview', object = out_obj, map = m)
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
            nm <- deparse(substitute(e2))
            m <- mapView(e2, map = e1, layer.name = nm,
                         map.types = getProviderTileNamesFromMap(e1))
            out_obj <- list(e2)
            if (length(e2) > 1) {
              ext <- raster::extent(
                raster::projectExtent(e2, crs = llcrs))
              m <- leaflet::fitBounds(map = m@map,
                                      lng1 = ext@xmin,
                                      lat1 = ext@ymin,
                                      lng2 = ext@xmax,
                                      lat2 = ext@ymax)
              #m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
            }
            out <- methods::new('mapview', object = out_obj, map = m)
            return(out)
          }
)
