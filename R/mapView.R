if ( !isGeneric('mapView') ) {
  setGeneric('mapView', function(x, ...)
    standardGeneric('mapView'))
}

#' View spatial objects interactively
#'
#' @description
#' this function produces an interactive view of the specified
#' spatial object(s) on top of the specified base maps.
#'
#' @details
#' If \code{zcol} is not \code{NULL} but a length one character vector
#' (referring to a column name of the attribute table)
#' and \code{burst} is \code{TRUE}, one layer for each unique value
#' of \code{zcol} will be drawn. The same will happen if \code{burst} is
#' a length one character vector (again referring to a column of
#' the attribute table). \cr
#' \cr
#' The usage of big data sets is performed by loading local copies
#' of json files from temporary storage. This works fine for most of
#' the current browsers. If you are using Google's chrome browser you have to
#' start the browser with the flag \code{-allow-file-access-from-files} (i.e
#' for windows: "path_to_your_chrome_installation\\chrome.exe --allow-file-access-from-files",
#' for linux: "/usr/bin/google-chrome --allow-access-from-files").
#' See \url{http://www.chrome-allow-file-access-from-file.com/} for further details.
#'
#' @param x a \code{Raster*} or \code{Spatial*} object.
#' @param map an optional existing map to be updated/added to
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param color color (palette) for points/polygons/lines
#' @param col.regions color (palette) pixels.
#' See \code{\link{levelplot}} for details.
#' @param at the breakpoints used for the visualisation.
#' See \code{\link{levelplot}} for details.
#' @param na.color color for missing values
#' @param use.layer.names should layer names of the Raster* object be used?
#' @param values a vector of values for the visualisation of the layers.
#' Per default these are calculated based on the supplied raster* object.
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param alpha opacity of the lines or points
#' @param alpha.regions opacity of the fills or the raster layer(s)
#' @param legend should a legend be plotted
#' @param legend.opacity opacity of the legend
#' @param trim should the raster be trimmed in case there are NAs on the egdes
#' @param verbose should some details be printed during the process
#' @param layer.name the name of the layer to be shown on the map
#' @param popup a \code{list} of HTML strings with the popup contents, usually
#' created from \code{\link{popupTable}}. See \code{\link{addControl}} for
#' details.
#' @param label a character vector of labels to be shown on mouseover. See
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
#' mapview()
#'
#' ### raster data ###
#' library(sp)
#' library(raster)
#'
#' # SpatialPixelsDataFrame
#' data(meuse.grid)
#' coordinates(meuse.grid) <- ~x+y
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' gridded(meuse.grid) <- TRUE
#'
#' mapView(meuse.grid, zcol = "soil")
#'
#' # raster stack
#' m1 <- mapview(poppendorf[[5]])
#' m1
#'
#' ### point vector data ###
#' ## SpatialPointsDataFrame ##
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#' # all layers of meuse
#' mapview(meuse, burst = TRUE)
#'
#' # only one layer, all info in popups
#' mapview(meuse)
#'
#' # one specific layer
#' mapview(meuse, zcol = "soil") # one layer
#' mapview(meuse, zcol = "soil", burst = TRUE) # three layers
#' mapview(meuse, burst = "soil") # identical to above
#'
#' ## SpatialPoints ##
#' meuse_pts <- as(meuse, "SpatialPoints")
#' mapview(meuse_pts)
#'
#'
#'
#' ### overlay vector on top of raster ###
#' mapview(meuse.grid, zcol = "ffreq") + meuse
#'
#' ### polygon vector data ###
#' data("gadmCHE")
#' m <- mapview(gadmCHE)
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
#' mapview(atlStorms2005)
#' mapview(atlStorms2005, burst = TRUE)
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
                   maxpixels = mapviewGetOption("mapview.maxpixels"),
                   col.regions = mapviewGetOption("raster.palette")(256),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   use.layer.names = FALSE,
                   values = NULL,
                   map.types = mapviewGetOption("basemaps"),
                   alpha.regions = 0.8,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = mapviewGetOption("verbose"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            if (is.null(at)) at <- lattice::do.breaks(
              lattice:::extend.limits(range(x[], na.rm = TRUE)), 256)

            if (mapviewGetOption("platform") == "leaflet") {
              leafletRL(x,
                        map = map,
                        maxpixels = maxpixels,
                        col.regions = col.regions,
                        at = at,
                        na.color, na.color,
                        use.layer.names = use.layer.names,
                        values = values,
                        map.types = map.types,
                        alpha.regions = alpha.regions,
                        legend = legend,
                        legend.opacity = legend.opacity,
                        trim = trim,
                        verbose = verbose,
                        layer.name = layer.name,
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
                   maxpixels = mapviewGetOption("mapview.maxpixels"),
                   col.regions = mapviewGetOption("raster.palette")(256),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   use.layer.names = TRUE,
                   values = NULL,
                   map.types = mapviewGetOption("basemaps"),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = mapviewGetOption("verbose"),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {
              leafletRSB(x,
                         map = map,
                         maxpixels = maxpixels,
                         col.regions = col.regions,
                         at = at,
                         na.color = na.color,
                         use.layer.names = use.layer.names,
                         values = values,
                         map.types = map.types,
                         legend = legend,
                         legend.opacity = legend.opacity,
                         trim = trim,
                         verbose = verbose,
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
                   map = NULL,
                   maxpixels = mapviewGetOption("mapview.maxpixels"),
                   col.regions = mapviewGetOption("raster.palette")(256),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   values = NULL,
                   map.types = mapviewGetOption("basemaps"),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = mapviewGetOption("verbose"),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {
              leafletSatellite(x,
                               map = map,
                               maxpixels = maxpixels,
                               col.regions = col.regions,
                               at = at,
                               na.color = na.color,
                               values = values,
                               map.types = map.types,
                               legend = legend,
                               legend.opacity = legend.opacity,
                               trim = trim,
                               verbose = verbose,
                               ...)
            } else {
              NULL
            }

          }

)


## SpatialPixelsDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialPixelsDataFrame}}
#'
setMethod('mapView', signature(x = 'SpatialPixelsDataFrame'),
          function(x,
                   zcol = NULL,
                   na.color = mapviewGetOption("na.color"),
                   legend = mapviewGetOption("legend"),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {
              leafletPixelsDF(x,
                              zcol = zcol,
                              na.color = na.color,
                              legend = legend,
                              ...)
            } else {
              NULL
            }

          }
)
## SpatialGridDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialGridDataFrame}}
#'
setMethod('mapView', signature(x = 'SpatialGridDataFrame'),
          function(x,
                   zcol = NULL,
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {
              leafletPixelsDF(as(x, "SpatialPixelsDataFrame"),
                              zcol,
                              ...)
            } else {
              NULL
            }

          }
)


## SpatialPointsDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialPointsDataFrame}}
#' @param burst whether to show all (TRUE) or only one (FALSE) layer(s).
#' See also Details.
#' @param zcol attribute name(s) or column number(s) in attribute table
#' of the column(s) to be rendered. See also Details.
#' @param cex attribute name(s) or column number(s) in attribute table
#' of the column(s) to be used for defining the size of circles
#' @param lwd line width

setMethod('mapView', signature(x = 'SpatialPointsDataFrame'),
          function(x,
                   map = NULL,
                   map.types = mapviewGetOption("basemaps"),
                   zcol = NULL,
                   burst = FALSE,
                   color = mapviewGetOption("vector.palette"),
                   alpha = 0.8,
                   col.regions = color,
                   alpha.regions = 0.2,
                   na.color = mapviewGetOption("na.color"),
                   at = NULL,
                   cex = 8,
                   lwd = 4,
                   popup = popupTable(x),
                   label,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   verbose = mapviewGetOption("verbose"),
                   ...) {

            if (nrow(coordinates(x)) < mapviewGetOption("maxpoints")) {
              if (mapviewGetOption("platform") == "leaflet") {
                leafletPointsDF(x,
                                map = map,
                                map.types = map.types,
                                zcol = zcol,
                                burst = burst,
                                color = color,
                                alpha = alpha,
                                col.regions = col.regions,
                                alpha.regions = alpha.regions,
                                na.color = na.color,
                                at = at,
                                cex = cex,
                                lwd = lwd,
                                popup = popup,
                                label = label,
                                legend = legend,
                                legend.opacity = legend.opacity,
                                layer.name = layer.name,
                                verbose = verbose,
                                ...)
              } else {
                NULL
              }
            } else {
              fpView(x,
                     zcol = zcol,
                     color = color,
                     na.color = na.color,
                     values = values,
                     map.types = map.types,
                     alpha = alpha,
                     weight = cex,
                     verbose = verbose,
                     layer.name = layer.name,
                     popup = popup,
                     )
            }

          }

)



## SpatialPoints ==========================================================
#' @describeIn mapView \code{\link{SpatialPoints}}

setMethod('mapView', signature(x = 'SpatialPoints'),
          function(x,
                   map = NULL,
                   zcol = NULL,
                   color = mapviewGetOption("vector.palette")(1),
                   na.color = mapviewGetOption("na.color"),
                   cex = 8,
                   lwd = 4,
                   alpha = 0.9,
                   alpha.regions = 0.4,
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label,
                  ...) {

            if (nrow(coordinates(x)) < mapviewGetOption("maxpoints")) {
              if (mapviewGetOption("platform") == "leaflet") {
                leafletPoints(x,
                              map = map,
                              color = color,
                              na.color = na.color,
                              cex = cex,
                              lwd = lwd,
                              alpha = alpha,
                              alpha.regions = alpha.regions,
                              map.types = map.types,
                              verbose = verbose,
                              layer.name = layer.name,
                              label = label,
                             ...)
              } else {
                NULL
              }
            } else {
              fpView(x,
                     zcol = NULL,
                     color = color,
                     na.color = na.color,
                     values = values,
                     map.types = map.types,
                     alpha = alpha,
                     weight = cex,
                     verbose = verbose,
                     layer.name = layer.name
                    )
            }


          }
)




## SpatialPolygonsDataFrame ===============================================
#' @describeIn mapView \code{\link{SpatialPolygonsDataFrame}}

setMethod('mapView', signature(x = 'SpatialPolygonsDataFrame'),
          function(x,
                   map = NULL,
                   map.types = mapviewGetOption("basemaps"),
                   zcol = NULL,
                   burst = FALSE,
                   color = mapviewGetOption("vector.palette"),
                   alpha = 0.8,
                   col.regions = color,
                   alpha.regions = 0.2,
                   na.color = mapviewGetOption("na.color"),
                   at = NULL,
                   cex = 8,
                   lwd = 2,
                   popup = popupTable(x),
                   label,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   verbose = mapviewGetOption("verbose"),
                   ...) {

            if (length(x@polygons) < mapviewGetOption("maxpolygons")) {
              if (mapviewGetOption("platform") == "leaflet") {
                leafletPolygonsDF(x,
                                  map = map,
                                  map.types = map.types,
                                  zcol = zcol,
                                  burst = burst,
                                  color = color,
                                  alpha = alpha,
                                  col.regions = col.regions,
                                  alpha.regions = alpha.regions,
                                  na.color = na.color,
                                  at = at,
                                  cex = cex,
                                  lwd = lwd,
                                  popup = popup,
                                  label = label,
                                  legend = legend,
                                  legend.opacity = legend.opacity,
                                  layer.name = layer.name,
                                  verbose = verbose,
                                  ...)
              } else {
                NULL
              }
            } else {
              bView(x,
                    zcol = NULL,
                    color = color,
                    na.color = na.color,
                    values = values,
                    map.types = map.types,
                    alpha.regions = alpha.regions,
                    lwd = lwd,
                    verbose = verbose,
                    layer.name = layer.name,
                    popup = NULL)
            }

          }

)



## SpatialPolygons ========================================================
#' @describeIn mapView \code{\link{SpatialPolygons}}

setMethod('mapView', signature(x = 'SpatialPolygons'),
          function(x,
                   map = NULL,
                   color = mapviewGetOption("vector.palette")(1),
                   na.color = mapviewGetOption("na.color"),
                   map.types = mapviewGetOption("basemaps"),
                   lwd = 2,
                   alpha = 0.8,
                   alpha.regions = 0.2,
                   verbose = mapviewGetOption("verbose"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label,
                  ...) {

            if (length(x@polygons) < mapviewGetOption("maxpolygons")) {
              if (mapviewGetOption("platform") == "leaflet") {
                leafletPolygons(x,
                                map = map,
                                color = color,
                                na.color = na.color,
                                lwd = lwd,
                                alpha = alpha,
                                alpha.regions = alpha.regions,
                                map.types = map.types,
                                verbose = verbose,
                                layer.name = layer.name,
                                label = label,
                               ...)
              } else {
                NULL
              }
            } else {
              bView(x,
                    zcol = NULL,
                    color = color,
                    na.color = na.color,
                    values = values,
                    map.types = map.types,
                    alpha.regions = alpha.regions,
                    lwd = lwd,
                    verbose = verbose,
                    layer.name = layer.name)
            }

          }
)


## SpatialLinesDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialLinesDataFrame}}

setMethod('mapView', signature(x = 'SpatialLinesDataFrame'),
          function(x,
                   map = NULL,
                   map.types = mapviewGetOption("basemaps"),
                   zcol = NULL,
                   burst = FALSE,
                   color = mapviewGetOption("vector.palette"),
                   alpha = 0.8,
                   col.regions = color,
                   alpha.regions = 0.2,
                   na.color = mapviewGetOption("na.color"),
                   at = NULL,
                   cex = 8,
                   lwd = 2,
                   popup = popupTable(x),
                   label,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   verbose = mapviewGetOption("verbose"),
                   ...) {

            if (length(x@lines) < mapviewGetOption("maxlines")) {
              if (mapviewGetOption("platform") == "leaflet") {
                leafletLinesDF(x,
                               map = map,
                               map.types = map.types,
                               zcol = zcol,
                               burst = burst,
                               color = color,
                               alpha = alpha,
                               col.regions = col.regions,
                               alpha.regions = alpha.regions,
                               na.color = na.color,
                               at = at,
                               cex = cex,
                               lwd = lwd,
                               popup = popup,
                               label = label,
                               legend = legend,
                               legend.opacity = legend.opacity,
                               layer.name = layer.name,
                               verbose = verbose,
                               ...)
              } else {
                NULL
              }
            } else {
              bView(x,
                    zcol = zcol,
                    color = color,
                    na.color = na.color,
                    values = values,
                    map.types = map.types,
                    alpha.regions = alpha,
                    lwd = lwd,
                    verbose = verbose,
                    layer.name = layer.name,
                    popup = NULL)
            }

          }

)




## SpatialLines ===========================================================
#' @describeIn mapView \code{\link{SpatialLines}}

setMethod('mapView', signature(x = 'SpatialLines'),
          function(x,
                   map = NULL,
                   zcol = NULL,
                   color = mapviewGetOption("vector.palette")(1),
                   na.color = mapviewGetOption("na.color"),
                   lwd = 2,
                   alpha = 0.8,
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label,
                  ...) {

            if (length(x@lines) < mapviewGetOption("maxlines")) {
              if (mapviewGetOption("platform") == "leaflet") {
                leafletLines(x,
                             map = map,
                             color = color,
                             na.color = na.color,
                             lwd = lwd,
                             alpha = alpha,
                             map.types = map.types,
                             verbose = verbose,
                             layer.name = layer.name,
                             label = label,
                            ...)
              } else {
                NULL
              }
            } else {
              bView(x,
                    zcol = zcol,
                    color = color,
                    na.color = na.color,
                    values = values,
                    map.types = map.types,
                    alpha.regions = alpha,
                    lwd = lwd,
                    verbose = verbose,
                    layer.name = layer.name)
            }

          }

)


## Missing ================================================================
#' @describeIn mapView initiate a map without an object
#' @param easter.egg well, you might find out if you set this to TRUE
#'
setMethod('mapView', signature(x = 'missing'),
          function(map.types = mapviewGetOption("basemaps"),
                   easter.egg = FALSE) {

            if (mapviewGetOption("platform") == "leaflet") {
              leafletMissing(map.types,
                             easter.egg)
            } else {
              NULL
            }

          }
)



## mapview ================================================================

if ( !isGeneric('mapview') ) {
  setGeneric('mapview', function(...)
    standardGeneric('mapview'))
}


#' @describeIn mapView alias for ease of typing
#' @aliases mapview
#' @export mapview
#'
setMethod('mapview', signature('ANY'),
          function(...) mapView(...))

