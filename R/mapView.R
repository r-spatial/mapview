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
#' @param highlight either \code{FALSE}, \code{NULL} or a list of styling
#' options for feature highlighting on mouse hover.
#' See \code{\link{highlightOptions}} for details.
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
#' ## simple features ====================================================
#' # sf
#' mapview(breweries)
#' mapview(franconia)
#'
#' # sfc
#' mapview(st_geometry(breweries)) # no popup
#'
#' # sfg / XY - taken from ?sf::st_point
#' outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#' hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#' hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#' pts = list(outer, hole1, hole2)
#' (pl1 = st_polygon(pts))
#' mapview(pl1)
#'
#' ## raster ==============================================================
#' mapview(poppendorf[[5]])
#'
#' ## spatial objects =====================================================
#' mapview(leaflet::gadmCHE)
#' mapview(atlStorms2005)
#'
#'
#' ## styling options & legends ===========================================
#' mapview(cantons, color = "white", col.regions = "red")
#' mapview(cantons, color = "magenta", col.regions = "white")
#'
#' mapview(breweries, zcol = "founded")
#' mapview(breweries, zcol = "founded", at = seq(1400, 2200, 200), legend = TRUE)
#' mapview(cantons, zcol = "NAME_1", legend = TRUE)
#'
#' library(RColorBrewer)
#' clrs <- colorRampPalette(brewer.pal(9, "Blues"))
#' mapview(breweries, zcol = "founded", col.regions = clrs, legend = TRUE)
#'
#' ### multiple layers ====================================================
#' mapview(franconia) + breweries
#' mapview(list(breweries, franconia))
#' mapview(breweries) + mapview(franconia) + stormtracks
#'
#' mapview(franconia, zcol = "district") + mapview(breweries, zcol = "village")
#' mapview(list(franconia, breweries),
#'         zcol = list("district", NULL),
#'         legend = list(TRUE, FALSE))
#'
#'
#' ### burst ==============================================================
#' mapview(franconia, burst = TRUE)
#' mapview(franconia, burst = TRUE, hide = TRUE)
#' mapview(franconia, zcol = "district", burst = TRUE)
#'
#'
#' ### in the pipe ========================================================
#' library(dplyr)
#' library(sf)
#'
#' franconia %>%
#'   sf::st_union() %>%
#'   mapview()
#'
#' franconia %>%
#'   group_by(district) %>%
#'   summarize() %>%
#'   mapview(zcol = "district")
#'
#' franconia %>%
#'   group_by(district) %>%
#'   summarize() %>%
#'   mutate(area = st_area(.) / 1e6) %>%
#'   mapview(zcol = "area")
#'
#' franconia %>%
#'   mutate(area = sf::st_area(.)) %>%
#'   mapview(zcol = "area", legend = TRUE)
#'
#' breweries %>%
#'   st_intersection(franconia) %>%
#'   mapview(zcol = "district")
#'
#' franconia %>%
#'   mutate(count = lengths(st_contains(., breweries))) %>%
#'   mapview(zcol = "count")
#'
#' franconia %>%
#'   mutate(count = lengths(st_contains(., breweries)),
#'          density = count / st_area(.)) %>%
#'   mapview(zcol = "density")
#'
#' }
#'
#' @export
#' @docType methods
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
              extendLimits(range(x[], na.rm = TRUE)), 256)

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



######## SIMPLE FEATURES ##################################################

## sf =====================================================================
#' @describeIn mapView \code{\link{st_sf}}

setMethod('mapView', signature(x = 'sf'),
          function(x,
                   map = NULL,
                   zcol = NULL,
                   burst = FALSE,
                   color = mapviewGetOption("vector.palette"),
                   col.regions = mapviewGetOption("vector.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   cex = 6,
                   lwd = lineWidth(x),
                   alpha = 0.9,
                   alpha.regions = regionOpacity(x),
                   map.types = NULL,
                   verbose = mapviewGetOption("verbose"),
                   popup = popupTable(x),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label = makeLabels(x, zcol),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   highlight = mapviewHighlightOptions(x, alpha.regions, lwd),
                   maxpoints = getMaxFeatures(x),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {

              tmp <- burst(x = x,
                           zcol = zcol,
                           burst = burst,
                           color = color,
                           col.regions = col.regions,
                           at = at,
                           na.color = na.color,
                           popup = popup)

              if (is.function(tmp)) {
                x <- tmp()$obj
                color <- tmp()$color
                col.regions <- tmp()$col.regions
                popup <- tmp()$popup
                label <- tmp()$labs
              }

              if (!inherits(x, "list")) {

                leaflet_sf(x,
                           map = map,
                           zcol = zcol,
                           color = color,
                           col.regions = col.regions,
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
                           highlight = highlight,
                           maxpoints = maxpoints,
                           ...)

              } else {

                mapView(x,
                        zcol = NULL,
                        burst = FALSE,
                        color = color,
                        col.regions = col.regions,
                        popup = popup,
                        label = label,
                        homebutton = homebutton,
                        legend = legend,
                        map.types = map.types,
                        ...)

              }

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
                   color = standardColor(x), #mapviewGetOption("vector.palette"),
                   col.regions = standardColRegions(x), #mapviewGetOption("vector.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   cex = 6,
                   lwd = lineWidth(x),
                   alpha = 0.9,
                   alpha.regions = regionOpacity(x),
                   map.types = NULL,
                   verbose = mapviewGetOption("verbose"),
                   popup = NULL,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame(2))),
                   label = makeLabels(x),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   highlight = mapviewHighlightOptions(x, alpha.regions, lwd),
                   maxpoints = getMaxFeatures(x),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {

              if (inherits(x, "sfc_GEOMETRY")) {
                mapview(lapply(split(x, st_dimension(x)), st_cast),
                        map = map,
                        color = color,
                        col.regions = col.regions,
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
                        highlight = highlight,
                        ...)
              } else {
                leaflet_sfc(x,
                            map = map,
                            color = color,
                            col.regions = col.regions,
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
                            highlight = highlight,
                            maxpoints = maxpoints,
                            ...)
              }
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
                   color = standardColor(x), #mapviewGetOption("vector.palette"),
                   col.regions = standardColRegions(x), #mapviewGetOption("vector.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   cex = 6,
                   lwd = lineWidth(x),
                   alpha = 0.9,
                   alpha.regions = regionOpacity(x),
                   map.types = NULL,
                   verbose = mapviewGetOption("verbose"),
                   popup = NULL,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame(1))),
                   label = makeLabels(x),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   highlight = mapviewHighlightOptions(x, alpha.regions, lwd),
                   maxpoints = getMaxFeatures(x),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {

              leaflet_sfc(x,
                          map = map,
                          color = color,
                          col.regions = col.regions,
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
                          highlight = highlight,
                          maxpoints = maxpoints,
                          ...)

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


## sfc_GEOMETRY =======================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'sfc_GEOMETRY'),
          function(x, ...) {
            callNextMethod()
          }
)


######## MISC #############################################################

## Missing ================================================================
#' @describeIn mapView initiate a map without an object
#'
setMethod('mapView', signature(x = 'missing'),
          function(map.types = mapviewGetOption("basemaps"), ...) {

            if (mapviewGetOption("platform") == "leaflet") {
              leafletMissing(map.types, ...)
            } else {
              NULL
            }

          }
)



## list ===================================================================
#' @describeIn mapView \code{\link{list}}
#'
setMethod('mapView', signature(x = 'list'),
          function(x,
                   map = NULL,
                   zcol = NULL,
                   color = mapviewGetOption("vector.palette"),
                   col.regions = mapviewGetOption("vector.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   cex = 6,
                   lwd = lapply(x, lineWidth),
                   alpha = 1,
                   alpha.regions = 0.6,
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   popup = lapply(seq(x), function(i) {
                     popupTable(x[[i]])
                   }),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label = lapply(seq(x), function(i) {
                     makeLabels(x[[i]], zcol = zcol[[i]])
                   }),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   highlight = lapply(seq(x), function(i) {
                     mapviewHighlightOptions(x[[i]],
                                             alpha.regions = alpha.regions,
                                             lwd = lwd[[i]])
                   }),
                   maxpoints = NULL, #lapply(x, getMaxFeatures),
                   ...) {

            makeLayerNames <- function(v1) {
              #chr <- as.character(dargs(graphics::plot.default)eparse(substitute(v1)))
              chr <- gsub(utils::glob2rx("*list(*"), "", v1)
              chr <- unlist(strsplit(x = gsub(")", "", chr), ","))
              as.list(gsub(" ", "", chr))
            }

            nms <- names(x)
            if (is.null(nms)) {
              lyrnms <- makeLayerNames(layer.name) #paste0("layer_", sprintf("%02.0f", seq(x)))
            } else {
              lyrnms <- nms
            }

            if (!is.list(color))
              color <- rep(list(color), length(x))
            if (!is.list(col.regions))
              col.regions <- rep(list(col.regions), length(x))
            if (!is.list(legend))
              legend <- rep(list(legend), length(x))
            if (!is.list(homebutton))
              homebutton <- rep(list(homebutton), length(x))
            if (!is.list(cex))
              cex <- rep(list(cex), length(x))
            if (!is.list(lwd))
              lwd <- rep(list(lwd), length(x))
            if (!is.list(highlight))
              highlight <- rep(list(highlight), length(x))
            # if (!is.list(label))
            #   label <- rep(list(label), length(x))
            if (length(popup) != length(x))
              popup <- rep(list(popup), length(x))

            if (mapviewGetOption("platform") == "leaflet") {
              m <- Reduce("+", lapply(seq(x), function(i) {
                if (is.null(popup)) popup <- popupTable(x[[i]])
                if (inherits(x[[i]], "sf")) {
                  mapView(x = x[[i]],
                          layer.name = lyrnms[[i]],
                          zcol = zcol[[i]],
                          color = color[[i]],
                          col.regions = col.regions[[i]],
                          legend = legend[[i]],
                          label = label[[i]],
                          popup = popup[[i]],
                          homebutton = homebutton[[i]],
                          native.crs = native.crs,
                          cex = cex[[i]],
                          lwd = lwd[[i]],
                          highlight = highlight[[i]],
                          map.types = map.types,
                          ...)
                  } else {
                    mapView(x = x[[i]],
                            layer.name = lyrnms[[i]],
                            homebutton = homebutton[[i]],
                            native.crs = native.crs,
                            cex = cex[[i]],
                            lwd = lwd[[i]],
                            highlight = highlight[[i]],
                            map.types = map.types,
                            ...)
                  }
                }))@map
              m <- leaflet::hideGroup(map = m,
                                      group = layers2bHidden(m, ...))
              out <- new("mapview", object = x, map = m)
              return(out)
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
#' @docType methods
#'
setMethod('mapview', signature('ANY'),
          function(...) mapView(...))


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
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            mapView(st_as_sf(x), layer.name = layer.name, ...)

          }

)


## SpatialPoints ==========================================================
#' @describeIn mapView \code{\link{SpatialPoints}}

setMethod('mapView', signature(x = 'SpatialPoints'),
          function(x,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            mapView(st_as_sfc(x), layer.name = layer.name, ...)

          }
)


## SpatialPolygonsDataFrame ===============================================
#' @describeIn mapView \code{\link{SpatialPolygonsDataFrame}}

setMethod('mapView', signature(x = 'SpatialPolygonsDataFrame'),
          function(x,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            mapView(st_as_sf(x), layer.name = layer.name, ...)

          }

)


## SpatialPolygons ========================================================
#' @describeIn mapView \code{\link{SpatialPolygons}}

setMethod('mapView', signature(x = 'SpatialPolygons'),
          function(x,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            mapView(st_as_sfc(x), layer.name = layer.name, ...)

          }
)


## SpatialLinesDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialLinesDataFrame}}

setMethod('mapView', signature(x = 'SpatialLinesDataFrame'),
          function(x,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            mapView(st_as_sf(x), layer.name = layer.name, ...)

          }

)


## SpatialLines ===========================================================
#' @describeIn mapView \code{\link{SpatialLines}}

setMethod('mapView', signature(x = 'SpatialLines'),
          function(x,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            mapView(st_as_sfc(x), layer.name = layer.name, ...)

          }

)

