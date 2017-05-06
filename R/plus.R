if ( !isGeneric('+') ) {
  setGeneric('+', function(x, y, ...)
    standardGeneric('+'))
}

#' mapview + mapview adds data from the second map to the first
#'
#' @param e1 a leaflet or mapview map to which e2 should be added.
#' @param e2 a (spatial) object to be added or a mapview object from which
#' the objects should be added to e1.
#'
#' @examples
#' \dontrun{
#' ### raster data ###
#' library(sp)
#' library(raster)
#'
#' m1 <- mapView(poppendorf[[5]])
#'
#' ### point vector data ###
#' m2 <- mapView(breweries91)
#'
#' ### add two mapview objects
#' m1 + m2 # final zoom level based on m2
#' '+'(m2, m1) # final zoom level based on m1
#' }
#'
#' @name +
#' @docType methods
#' @rdname plus
#' @aliases +,mapview,mapview-method

setMethod("+",
          signature(e1 = "mapview",
                    e2 = "mapview"),
          function (e1, e2)
          {
            # is.ext <- class(e2@object[[length(e2@object)]]) == "Extent"
            # if (is.ext) {
            #   rst <- raster::raster(e2@object[[length(e2@object)]])
            #   raster::projection(rst) <- llcrs
            # }
            m <- e1@map
            m <- appendMapCallEntries(m, e2@map)
            out_obj <- append(e1@object, e2@object)
            ext <- createExtent(out_obj[[length(out_obj)]])
            # if (length(e2@object[[length(e2@object)]]) > 1) {
            #   if (is.ext) ext <- raster::extent(rst) else
            #     ext <- raster::extent(
            #       raster::projectExtent(out_obj[[length(out_obj)]],
            #                             crs = llcrs))
              m <- leaflet::fitBounds(map = m,
                                      lng1 = ext@xmin,
                                      lat1 = ext@ymin,
                                      lng2 = ext@xmax,
                                      lat2 = ext@ymax)
              #m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
            # }

            out <- methods::new('mapview', object = out_obj, map = m)
            return(out)
          }
)

#' mapview + data adds spatial data (raster*, sf*, sp*) to a mapview map
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
            ext <- createExtent(out_obj[[length(out_obj)]])
            m <- leaflet::fitBounds(map = m@map,
                                    lng1 = ext@xmin,
                                    lat1 = ext@ymin,
                                    lng2 = ext@xmax,
                                    lat2 = ext@ymax)
            out <- methods::new('mapview', object = out_obj, map = m)
            return(out)
          }
)

# #' @name +
# #' @docType methods
# #' @rdname plus
# #' @aliases +,leaflet,ANY-method
#'
# setMethod("+",
#           signature(e1 = "leaflet",
#                     e2 = "ANY"),
#           function (e1, e2)
#           {
#
#             nm <- deparse(substitute(e2))
#             m <- mapView(e2, map = e1, layer.name = nm,
#                          map.types = getProviderTileNamesFromMap(e1))
#             out_obj <- list(e2)
#             ext <- createExtent(e2)
#             m <- leaflet::fitBounds(map = m@map,
#                                     lng1 = ext@xmin,
#                                     lat1 = ext@ymin,
#                                     lng2 = ext@xmax,
#                                     lat2 = ext@ymax)
#             out <- methods::new('mapview', object = out_obj, map = m)
#             return(out)
#           }
# )

#' [...]
#' @name +
#' @docType methods
#' @rdname plus
#' @aliases +,mapview,character-method
#'
setMethod("+",
          signature(e1 = "mapview",
                    e2 = "character"),
          function (e1, e2) {

            if (e2 %in% c("easteregg", "easter.egg", "easter_egg",
                          "easter", "easterEgg", "EasterEgg", "eegg",
                          "easter", "egg", "Easter", "Egg", "Nobody",
                          "Terence Hill", "trinity", "Trinity",
                          "easter egg", "Easter Egg", "Easter egg")) {
              cat("\nBehold! Someone's drawing quicker than the rest...\n\n")
              mapView(easter.egg = TRUE)
            } else {
              stop("\n\nSorry, but there seems to be someone who draws quicker than you...\n\n
                   Try again!")
            }

          }
)

