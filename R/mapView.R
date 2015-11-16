if ( !isGeneric('mapView') ) {
  setGeneric('mapView', function(x, ...)
    standardGeneric('mapView'))
}

#' View spatial objects interactively
#'
#' @description
#' this function produces an interactive GIS-like view of the specified
#' spatial object(s) on top of the specified base maps.
#'
#' @param x a \code{\link{raster}}* object
#' @param map an optional existing map to be updated/added to
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param color color (palette) of the points/polygons/lines/pixels
#' @param na.color color for missing values
#' @param use.layer.names should layer names of the Raster* object be used?
#' @param values a vector of values for the visualisation of the layers.
#' Per default these are calculated based on the supplied raster* object.
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param layer.opacity opacity of the raster layer(s)
#' @param legend should a legend be plotted
#' @param legend.opacity opacity of the legend
#' @param trim should the raster be trimmed in case there are NAs on the egdes
#' @param verbose should some details be printed during the process
#' @param layer.name the name of the layer to be shown on the map
#' @param popup a character vector of the HTML content for the popups. See
#' \code{\link{addControl}} for details.
#' @param ... additional arguments passed on to repective functions.
#' See \code{\link{addRasterImage}}, \code{\link{addCircles}},
#' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' \dontrun{
#' mapView()
#'
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
#' # raster stack
#' m1 <- mapView(meuse_rst)
#' m1
#'
#' # factorial RasterLayer
#' m2 <- mapView(raster::as.factor(meuse_rst[[4]]))
#' m2
#'
#' # SpatialPixelsDataFrame
#' mapView(meuse.grid, zcol = "soil")
#'
#'
#' ### point vector data ###
#' ## SpatialPointsDataFrame ##
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#' # all layers of meuse
#' mapView(meuse, burst = TRUE)
#'
#' # only one layer, all info in popups
#' mapView(meuse)
#'
#' ## SpatialPoints ##
#' meuse_pts <- as(meuse, "SpatialPoints")
#' mapView(meuse_pts)
#'
#'
#'
#' ### overlay vector on top of raster ###
#' mapView(meuse.grid, zcol = "ffreq") + meuse
#'
#' ### polygon vector data ###
#' data("gadmCHE")
#' m <- mapView(gadmCHE)
#' m
#'
#' ## points on polygons ##
#' centres <- data.frame(coordinates(gadmCHE))
#' names(centres) <- c("x", "y")
#' coordinates(centres) <- ~ x + y
#' projection(centres) <- projection(gadmCHE)
#' m + centres
#'
#' ### lines vector data
#' data("atlStorms2005")
#' mapView(atlStorms2005)
#' mapView(atlStorms2005, burst = TRUE)
#' }
#'
#' @export mapView
#' @name mapView
#' @rdname mapView
#' @aliases mapView,RasterLayer-method

## RasterLayer ============================================================

setMethod('mapView', signature(x = 'RasterLayer'),
          function(x,
                   map = NULL,
                   maxpixels = mapviewOptions(console = FALSE)$maxpixels,
                   color = mapViewPalette(7),
                   na.color = mapviewOptions(console = FALSE)$nacolor,
                   use.layer.names = FALSE,
                   values = NULL,
                   map.types = mapviewOptions(console = FALSE)$basemaps,
                   layer.opacity = 0.8,
                   legend = TRUE,
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = mapviewOptions(console = FALSE)$verbose,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            if (mapviewOptions(console = FALSE)$platform == "leaflet") {
              leafletRL(x,
                        map,
                        maxpixels,
                        color,
                        na.color,
                        use.layer.names,
                        values,
                        map.types,
                        layer.opacity,
                        legend,
                        legend.opacity,
                        trim,
                        verbose,
                        layer.name,
                        ...)
            } else {
              NULL
            }

          }

)

## Raster Stack/Brick ===========================================================
#' @describeIn mapView \code{\link{stack}} / \code{\link{brick}}

setMethod('mapView', signature(x = 'RasterStackBrick'),
          function(x,
                   map = NULL,
                   maxpixels = mapviewOptions(console = FALSE)$maxpixels,
                   color = mapViewPalette(7),
                   na.color = mapviewOptions(console = FALSE)$nacolor,
                   values = NULL,
                   map.types = mapviewOptions(console = FALSE)$basemaps,
                   legend = TRUE,
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = mapviewOptions(console = FALSE)$verbose,
                   ...) {

            if (mapviewOptions(console = FALSE)$platform == "leaflet") {
              leafletRSB(x,
                         map,
                         maxpixels,
                         color,
                         na.color,
                         values,
                         map.types,
                         legend,
                         legend.opacity,
                         trim,
                         verbose,
                         ...)
            } else {
              NULL
            }

          }
)



## Satellite object =======================================================
#' @describeIn mapView \code{\link{satellite}}

setMethod('mapView', signature(x = 'Satellite'),
          function(x,
                   ...) {

            pkgs <- c("leaflet", "satellite", "magrittr")
            tst <- sapply(pkgs, "requireNamespace",
                          quietly = TRUE, USE.NAMES = FALSE)

            lyrs <- x@layers

            m <- mapView(lyrs[[1]], ...)

            if (length(lyrs) > 1) {
              for (i in 2:length(lyrs)) {
                m <- mapView(lyrs[[i]], m, ...)
              }
            }

            if (length(getLayerNamesFromMap(m)) > 1) {
              m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
            }

            out <- new('mapview', object = list(x), map = m)

            return(out)

          }

)


## SpatialPixelsDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialPixelsDataFrame}}
#'
setMethod('mapView', signature(x = 'SpatialPixelsDataFrame'),
          function(x,
                   zcol = NULL,
                   ...) {

            if (mapviewOptions(console = FALSE)$platform == "leaflet") {
              leafletPixelsDF(x,
                              zcol,
                              ...)
            } else {
              NULL
            }

          }
)


## SpatialPointsDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialPointsDataFrame}}
#' @param burst whether to show all (TRUE) or only one (FALSE) layers
#' @param zcol attribute name(s) or column number(s) in attribute table
#' of the column(s) to be rendered
#' @param radius attribute name(s) or column number(s) in attribute table
#' of the column(s) to be used for defining the size of circles

setMethod('mapView', signature(x = 'SpatialPointsDataFrame'),
          function(x,
                   zcol = NULL,
                   map = NULL,
                   burst = FALSE,
                   color = mapViewPalette(7),
                   na.color = mapviewOptions(console = FALSE)$nacolor,
                   radius = 10,
                   map.types = mapviewOptions(console = FALSE)$basemaps,
                   legend = TRUE,
                   legend.opacity = 1,
                   verbose = mapviewOptions(console = FALSE)$verbose,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   popup = NULL,
                   ...) {

            if (mapviewOptions(console = FALSE)$platform == "leaflet") {
              leafletPointsDF(x,
                              zcol,
                              map,
                              burst,
                              color,
                              na.color,
                              radius,
                              map.types,
                              legend,
                              legend.opacity,
                              verbose,
                              layer.name,
                              popup,
                              ...)
            } else {
              NULL
            }

          }

)



## SpatialPoints ==========================================================
#' @describeIn mapView \code{\link{SpatialPoints}}

setMethod('mapView', signature(x = 'SpatialPoints'),
          function(x,
                   map = NULL,
                   na.color = mapviewOptions(console = FALSE)$nacolor,
                   map.types = mapviewOptions(console = FALSE)$basemaps,
                   verbose = mapviewOptions(console = FALSE)$verbose,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            if (mapviewOptions(console = FALSE)$platform == "leaflet") {
              leafletPoints(x,
                            map,
                            na.color,
                            map.types,
                            verbose,
                            layer.name,
                            ...)
            } else {
              NULL
            }

          }
)




## SpatialPolygonsDataFrame ===============================================
#' @describeIn mapView \code{\link{SpatialPolygonsDataFrame}}
#' @param weight line width (see \code{\link{leaflet}} for details)

setMethod('mapView', signature(x = 'SpatialPolygonsDataFrame'),
          function(x,
                   zcol = NULL,
                   map = NULL,
                   burst = FALSE,
                   color = mapViewPalette(7),
                   na.color = mapviewOptions(console = FALSE)$nacolor,
                   values = NULL,
                   map.types = mapviewOptions(console = FALSE)$basemaps,
                   legend = TRUE,
                   legend.opacity = 1,
                   weight = 2,
                   verbose = mapviewOptions(console = FALSE)$verbose,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   popup = NULL,
                   ...) {

            if (mapviewOptions(console = FALSE)$platform == "leaflet") {
              leafletPolygonsDF(x,
                                zcol,
                                map,
                                burst,
                                color,
                                na.color,
                                values,
                                map.types,
                                legend,
                                legend.opacity,
                                weight,
                                verbose,
                                layer.name,
                                popup,
                                ...)
            } else {
              NULL
            }

          }

)



## SpatialPolygons ========================================================
#' @describeIn mapView \code{\link{SpatialPolygons}}

setMethod('mapView', signature(x = 'SpatialPolygons'),
          function(x,
                   map = NULL,
                   na.color = mapviewOptions(console = FALSE)$nacolor,
                   map.types = mapviewOptions(console = FALSE)$basemaps,
                   weight = 2,
                   verbose = mapviewOptions(console = FALSE)$verbose,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            if (mapviewOptions(console = FALSE)$platform == "leaflet") {
              leafletPolygons(x,
                              map,
                              na.color,
                              map.types,
                              weight,
                              verbose,
                              layer.name,
                              ...)
            } else {
              NULL
            }

          }
)


## SpatialLinesDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialLinesDataFrame}}

setMethod('mapView', signature(x = 'SpatialLinesDataFrame'),
          function(x,
                   zcol = NULL,
                   map = NULL,
                   burst = FALSE,
                   color = mapViewPalette(7),
                   na.color = mapviewOptions(console = FALSE)$nacolor,
                   values = NULL,
                   map.types = mapviewOptions(console = FALSE)$basemaps,
                   legend = TRUE,
                   legend.opacity = 1,
                   verbose = mapviewOptions(console = FALSE)$verbose,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   popup = NULL,
                   ...) {

            if (mapviewOptions(console = FALSE)$platform == "leaflet") {
              leafletLinesDF(x,
                             zcol,
                             map,
                             burst,
                             color,
                             na.color,
                             values,
                             map.types,
                             legend,
                             legend.opacity,
                             verbose,
                             layer.name,
                             popup,
                             ...)
            } else {
              NULL
            }

          }

)




## SpatialLines ===========================================================
#' @describeIn mapView \code{\link{SpatialLines}}

setMethod('mapView', signature(x = 'SpatialLines'),
          function(x,
                   map = NULL,
                   na.color = mapviewOptions(console = FALSE)$nacolor,
                   map.types = mapviewOptions(console = FALSE)$basemaps,
                   verbose = mapviewOptions(console = FALSE)$verbose,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            if (mapviewOptions(console = FALSE)$platform == "leaflet") {
              leafletLines(x,
                           map,
                           na.color,
                           map.types,
                           verbose,
                           layer.name,
                           ...)
            } else {
              NULL
            }

          }

)


## Missing ================================================================
#' @describeIn mapView initiate a map without an object
#'
#' @param easter.egg well, you might find out if you set this to TRUE
setMethod('mapView', signature(x = 'missing'),
          function(map.types = mapviewOptions(console = FALSE)$basemaps,
                   easter.egg = FALSE) {

            if (mapviewOptions(console = FALSE)$platform == "leaflet") {
              leafletMissing(map.types,
                             easter.egg)
            } else {
              NULL
            }

          }
)


## mapview ================================================================
#' @export mapview
#'
mapview <- function(...) mapView(...)
