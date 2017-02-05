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
#' @param x a \code{Raster*} or \code{Spatial*} or \code{Satellite} or
#' \code{sf} object or a list of any combination of those.
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
#' @param homebutton logical, whether to add a zoom-to-layer button to the map.
#' Defaults to TRUE
#' @param popup a \code{list} of HTML strings with the popup contents, usually
#' created from \code{\link{popupTable}}. See \code{\link{addControl}} for
#' details.
#' @param label a character vector of labels to be shown on mouseover. See
#' \code{\link{addControl}} for details.
#' @param native.crs logical whether to reproject to web map coordinate
#' reference system (web mercator - epsg:3857) or render using native CRS of
#' the supplied data (can also be NA). Default is FALSE which will render in
#' web mercator. If set to TRUE now background maps will be drawn (but rendering
#' may be much quicker as no reprojecting is necessary). Currently only works
#' for simple features.
#' @param highlightOptions a list of styling options for feature highlighting
#' on mouse hover. See \code{\link{highlightOptions}} for details.
#' @param maxpoints the maximum number of points making up the geometry.
#' In case of lines and polygons this refers to the number of vertices. See
#' Details for more information.
#' @param ... additional arguments passed on to repective functions.
#' See \code{\link{addRasterImage}}, \code{\link{addCircles}},
#' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
#'
#' @details
#' \code{maxpoints} is taken to determine when to switch rendering from svg
#' to canvas overlay for perfomance. The threshold calculation is done as follows: \cr
#' if the number of points (in case of point data) or vertices (in case of
#' polygon or line data) > \code{maxpoints} then render using special render
#' function. Within this render function we approximate the complexity of
#' fetures by \cr
#' \cr
#' \code{maxFeatures <- maxfeatures / (npts(data) / length(data))} \cr
#' \cr
#' where \code{npts} determines the umber of points/vertices and \code{length}
#' the number of features (points, lines or polygons). When the number of
#' fetures in the current view window is larger than \code{maxFeatures} then
#' features are rendered on the canvas, otherwise they are rendered as svg objects
#' and fully queriable.
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
#'
#'
#' ### arbitrary CRS (only for simple features)
#' library(sf)
#' library(sp)
#'
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#' meuse_sf <- st_as_sf(meuse)
#'
#' mapview(meuse_sf, native.crs = TRUE)
#'
#' }
#'
#' @export mapView
#' @name mapView
#' @rdname mapView
#' @aliases mapView,RasterLayer-method

######## RASTER ###########################################################

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
                   homebutton = TRUE,
                   ...) {

            if (is.null(at)) at <- lattice::do.breaks(
              lattice:::extend.limits(range(x[], na.rm = TRUE)), 256)

            if (mapviewGetOption("platform") == "leaflet") {
              # if (maxpixels < raster::ncell(x)) {
              #   plainview(x,
              #             maxpixels = maxpixels,
              #             col.regions = col.regions,
              #             at = at,
              #             na.color = na.color,
              #             verbose = verbose,
              #             layer.name = layer.name,
              #             ...)
              # } else {
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
                          homebutton = homebutton,
                          ...)
            } else {
              if (mapviewGetOption("platform") == "quickmapr") {
                quickmapr::qmap(x, ...)
              } else {
                NULL
              }
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
                   homebutton = TRUE,
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
                         homebutton = homebutton,
                         ...)
            } else {
              if (mapviewGetOption("platform") == "quickmapr") {
                quickmapr::qmap(x, ...)
              } else {
                NULL
              }
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
                   homebutton = TRUE,
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
                               homebutton = homebutton,
                               ...)
            } else {
              NULL
            }

          }

)


######## SP ###############################################################

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
              if (mapviewGetOption("platform") == "quickmapr") {
                quickmapr::qmap(x, ...)
              } else {
                NULL
              }
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
              if (mapviewGetOption("platform") == "quickmapr") {
                quickmapr::qmap(x, ...)
              } else {
                NULL
              }
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
                   label = NULL,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   verbose = mapviewGetOption("verbose"),
                   homebutton = TRUE,
                   ...) {

            # if (nrow(x) < mapviewGetOption("maxpoints")) {
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
                                homebutton = homebutton,
                                ...)
              } else {
                if (mapviewGetOption("platform") == "quickmapr") {
                  quickmapr::qmap(x, ...)
                } else {
                  NULL
                }
              }
            # } else {
            #   fpView(x,
            #          zcol = zcol,
            #          color = color,
            #          na.color = na.color,
            #          values = values,
            #          map.types = map.types,
            #          alpha = alpha,
            #          weight = cex,
            #          verbose = verbose,
            #          layer.name = layer.name,
            #          popup = popup,
            #          )
            # }

          }

)



## SpatialPoints ==========================================================
#' @describeIn mapView \code{\link{SpatialPoints}}

setMethod('mapView', signature(x = 'SpatialPoints'),
          function(x,
                   map = NULL,
                   zcol = NULL,
                   color = mapviewGetOption("vector.palette"),
                   na.color = mapviewGetOption("na.color"),
                   cex = 8,
                   lwd = 4,
                   alpha = 0.9,
                   alpha.regions = 0.4,
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label = NULL,
                   homebutton = TRUE,
                  ...) {

            # if (nrow(coordinates(x)) < mapviewGetOption("maxpoints")) {
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
                              homebutton = homebutton,
                             ...)
              } else {
                if (mapviewGetOption("platform") == "quickmapr") {
                  quickmapr::qmap(x, ...)
                } else {
                  NULL
                }
              }
            # } else {
            #   fpView(x,
            #          zcol = NULL,
            #          color = color,
            #          na.color = na.color,
            #          values = values,
            #          map.types = map.types,
            #          alpha = alpha,
            #          weight = cex,
            #          verbose = verbose,
            #          layer.name = layer.name
            #         )
            # }


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
                   label = NULL,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   verbose = mapviewGetOption("verbose"),
                   homebutton = TRUE,
                   ...) {

            # if (length(x@polygons) < mapviewGetOption("maxpolygons")) {
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
                                  homebutton = homebutton,
                                  ...)
              } else {
                if (mapviewGetOption("platform") == "quickmapr") {
                  quickmapr::qmap(x, ...)
                } else {
                  NULL
                }
              }
            # } else {
            #   bView(x,
            #         zcol = NULL,
            #         color = color,
            #         na.color = na.color,
            #         values = values,
            #         map.types = map.types,
            #         alpha.regions = alpha.regions,
            #         lwd = lwd,
            #         verbose = verbose,
            #         layer.name = layer.name,
            #         popup = NULL)
            # }

          }

)



## SpatialPolygons ========================================================
#' @describeIn mapView \code{\link{SpatialPolygons}}

setMethod('mapView', signature(x = 'SpatialPolygons'),
          function(x,
                   map = NULL,
                   color = mapviewGetOption("vector.palette"),
                   na.color = mapviewGetOption("na.color"),
                   map.types = mapviewGetOption("basemaps"),
                   lwd = 2,
                   alpha = 0.8,
                   alpha.regions = 0.2,
                   verbose = mapviewGetOption("verbose"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label = NULL,
                   homebutton = TRUE,
                  ...) {

            # if (length(x@polygons) < mapviewGetOption("maxpolygons")) {
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
                                homebutton = homebutton,
                               ...)
              } else {
                if (mapviewGetOption("platform") == "quickmapr") {
                  quickmapr::qmap(x, ...)
                } else {
                  NULL
                }
              }
            # } else {
            #   bView(x,
            #         zcol = NULL,
            #         color = color,
            #         na.color = na.color,
            #         values = values,
            #         map.types = map.types,
            #         alpha.regions = alpha.regions,
            #         lwd = lwd,
            #         verbose = verbose,
            #         layer.name = layer.name)
            # }

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
                   label = NULL,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   verbose = mapviewGetOption("verbose"),
                   homebutton = TRUE,
                   ...) {

            # if (length(x@lines) < mapviewGetOption("maxlines")) {
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
                               homebutton = homebutton,
                               ...)
              } else {
                if (mapviewGetOption("platform") == "quickmapr") {
                  quickmapr::qmap(x, ...)
                } else {
                  NULL
                }
              }
            # } else {
            #   bView(x,
            #         zcol = zcol,
            #         color = color,
            #         na.color = na.color,
            #         values = values,
            #         map.types = map.types,
            #         alpha.regions = alpha,
            #         lwd = lwd,
            #         verbose = verbose,
            #         layer.name = layer.name,
            #         popup = NULL)
            # }

          }

)




## SpatialLines ===========================================================
#' @describeIn mapView \code{\link{SpatialLines}}

setMethod('mapView', signature(x = 'SpatialLines'),
          function(x,
                   map = NULL,
                   zcol = NULL,
                   color = mapviewGetOption("vector.palette"),
                   na.color = mapviewGetOption("na.color"),
                   lwd = 2,
                   alpha = 0.8,
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label = NULL,
                   homebutton = TRUE,
                  ...) {

            # if (length(x@lines) < mapviewGetOption("maxlines")) {
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
                             homebutton = homebutton,
                            ...)
              } else {
                if (mapviewGetOption("platform") == "quickmapr") {
                  quickmapr::qmap(x, ...)
                } else {
                  NULL
                }
              }
            # } else {
            #   bView(x,
            #         zcol = zcol,
            #         color = color,
            #         na.color = na.color,
            #         values = values,
            #         map.types = map.types,
            #         alpha.regions = alpha,
            #         lwd = lwd,
            #         verbose = verbose,
            #         layer.name = layer.name)
            # }

          }

)

######## SIMPLE FEATURES ##################################################

## sf =====================================================================
#' @describeIn mapView \code{\link{st_sf}}

setMethod('mapView', signature(x = 'sf'),
          function(x,
                   map = NULL,
                   zcol = NULL,
                   color = mapviewGetOption("vector.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   cex = 8,
                   lwd = 2,
                   alpha = 0.8,
                   alpha.regions = 0.2,
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   popup = popupTable(x),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label = makeLabels(x, zcol),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   highlightOptions = mapviewHighlightOptions(x, ...),
                   maxpoints = getMaxFeatures(x),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {

              leaflet_sf(x,
                         map = map,
                         zcol = zcol,
                         color = color,
                         at = at,
                         na.color = na.color,
                         cex = cex,
                         lwd = lwd,
                         alpha = alpha,
                         alpha.regions = alpha.regions,
                         map.types = map.types,
                         verbose = verbose,
                         popup = popup,
                         layer.name = layer.name,
                         label = label,
                         legend = legend,
                         legend.opacity = legend.opacity,
                         homebutton = homebutton,
                         native.crs = native.crs,
                         highlightOptions = highlightOptions,
                         maxpoints = maxpoints,
                         ...)

            } else if (mapviewGetOption("platform") == "quickmapr") {
              quickmapr::qmap(x, ...)
            } else {
              NULL
            }
          }
)


## sfc ====================================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'sfc'),
          function(x,
                   map = NULL,
                   color = "#6666ff",
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   cex = 8,
                   lwd = 2,
                   alpha = 0.8,
                   alpha.regions = 0.2,
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   popup = NULL,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame(2))),
                   label = makeLabels(x),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   highlightOptions = mapviewHighlightOptions(x, ...),
                   maxpoints = getMaxFeatures(x),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {

              leaflet_sfc(x,
                          map = map,
                          color = color,
                          na.color = na.color,
                          cex = cex,
                          lwd = lwd,
                          alpha = alpha,
                          alpha.regions = alpha.regions,
                          map.types = map.types,
                          verbose = verbose,
                          popup = popup,
                          layer.name = layer.name,
                          label = label,
                          legend = legend,
                          legend.opacity = legend.opacity,
                          homebutton = homebutton,
                          native.crs = native.crs,
                          highlightOptions = highlightOptions,
                          maxpoints = maxpoints,
                          ...)

            } else if (mapviewGetOption("platform") == "quickmapr") {
              quickmapr::qmap(x, ...)
            } else {
              NULL
            }
          }
)


## XY =====================================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'XY'),
          function(x,
                   map = NULL,
                   color = "#6666ff",
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   cex = 8,
                   lwd = 2,
                   alpha = 0.8,
                   alpha.regions = 0.2,
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   popup = NULL,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame(1))),
                   label = makeLabels(x),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   highlightOptions = mapviewHighlightOptions(x, ...),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {

              leaflet_sfc(x,
                          map = map,
                          color = color,
                          na.color = na.color,
                          cex = cex,
                          lwd = lwd,
                          alpha = alpha,
                          alpha.regions = alpha.regions,
                          map.types = map.types,
                          verbose = verbose,
                          popup = popup,
                          layer.name = layer.name,
                          label = label,
                          legend = legend,
                          legend.opacity = legend.opacity,
                          homebutton = homebutton,
                          native.crs = native.crs,
                          highlightOptions = highlightOptions,
                          ...)

            } else if (mapviewGetOption("platform") == "quickmapr") {
              quickmapr::qmap(x, ...)
            } else {
              NULL
            }
          }
)


## sfc_POINT ==============================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'sfc_POINT'),
          function(x, ...) {
            callNextMethod()
          }
)


## sfc_MULTIPOINT =========================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'sfc_MULTIPOINT'),
          function(x, ...) {
            callNextMethod()
          }
)


## sfc_LINESTRING =========================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'sfc_LINESTRING'),
          function(x, ...) {
            callNextMethod()
          }
)


## sfc_MULTILINESTRING ====================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'sfc_MULTILINESTRING'),
          function(x, ...) {
            callNextMethod()
          }
)


## sfc_POLYGON =======================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'sfc_POLYGON'),
          function(x, ...) {
            callNextMethod()
          }
)


## sfc_MULTIPOLYGON =======================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'sfc_MULTIPOLYGON'),
          function(x, ...) {
            callNextMethod()
          }
)



######## MISC #############################################################

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



## list ===================================================================
#' @describeIn mapView \code{\link{list}}
#'
setMethod('mapView', signature(x = 'list'),
          function(x, ...) {
            nms <- names(x)
            if (is.null(nms)) {
              lyrnms <- paste0("layer_", sprintf("%02.0f", seq(x)))
            } else {
              lyrnms <- nms
            }
            if (mapviewGetOption("platform") == "leaflet") {
              Reduce("+", lapply(seq(x), function(i) {
                mapView(x = x[[i]], layer.name = lyrnms[i], ...)
              }))
            } else {
              if (mapviewGetOption("platform") == "quickmapr") {
                quickmapr::qmap(x, ...)
              } else {
                NULL
              }
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

