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
#' NOTE: if XYZ or XYM or XYZM data from package sf is passed to mapview,
#' dimensions Z and M will be stripped to ensure smooth rendering even though
#' the popup will potentially still say something like "POLYGON Z".
#'
#' @param x a \code{Raster*} or \code{Spatial*} or \code{Satellite} or
#' \code{sf} object or a list of any combination of those. Furthermore,
#'   this can also be a \code{data.frame}, a \code{numeric vector} or a
#' \code{character string} pointing to a tile image folder or file on disk.
#'   If missing, a blank map will be drawn. A value of NULL will return NULL.
#' @param map an optional existing map to be updated/added to.
#' @param band for stars layers, the band number to be plotted.
#' @param pane name of the map pane in which to render features. See
#' \code{\link{addMapPane}} for details. Currently only supported for vector layers.
#'   Ignored if \code{canvas = TRUE}. The default \code{"auto"} will create different panes
#'   for points, lines and polygons such that points overlay lines overlay polygons.
#'   Set to \code{NULL} to get default leaflet behaviour where allfeatures
#'   are rendered in the same pane and layer order is determined automatically/sequentially.
#' @param canvas whether to use canvas rendering rather than svg. May help
#'   performance with larger data. See \url{https://leafletjs.com/reference-1.6.0.html#canvas}
#'   for more information. Only applicable for vector data. The default setting will
#'   decide automatically, based on feature complexity.
#' @param viewer.suppress whether to render the map in the browser (\code{TRUE})
#'   or the RStudio viewer (\code{FALSE}). When not using RStudio, maps will open
#'   in the browser by default. This is passed to \link[htmlwidgets]{sizingPolicy}
#'   via \link[leaflet]{leafletSizingPolicy}. For raster data the default is \code{FALSE}.
#'   For vector data it depends on argument \code{canvas}.
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#'   If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param color color (palette) for points/polygons/lines
#' @param col.regions color (palette) pixels.
#'   See \code{\link{levelplot}} for details.
#' @param at the breakpoints used for the visualisation.
#'   See \code{\link{levelplot}} for details.
#' @param na.color color for missing values
#' @param use.layer.names should layer names of the Raster* object be used?
#' @param map.types character spcifications for the base maps.
#'   see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#'   for available options.
#' @param burst whether to show all (TRUE) or only one (FALSE) layer(s).
#'   See also Details.
#' @param zcol attribute name(s) or column number(s) in attribute table
#'   of the column(s) to be rendered. See also Details.
#' @param cex attribute name(s) or column number(s) in attribute table
#'   of the column(s) to be used for defining the size of circles
#' @param lwd line width
#' @param alpha opacity of lines
#' @param alpha.regions opacity of the fills of points, polygons or raster layer(s)
#' @param na.alpha opacity of missing values
#' @param legend should a legend be plotted
#' @param legend.opacity opacity of the legend
#' @param trim should the raster be trimmed in case there are NAs on the edges
#' @param verbose should some details be printed during the process
#' @param layer.name the name of the layer to be shown on the map.
#'   By default this is the character version of whatever is passed to \code{x}.
#'   NOTE: This is being passed to underlying leaflet functions as the group argument.
#'   So if you use mapview to set up a map and want to refer to a certain layer
#'   later on, this is what you should refer to in \code{group}.
#' @param homebutton logical, whether to add a zoom-to-layer button to the map.
#'   Defaults to TRUE
#' @param popup a \code{list} of HTML strings with the popup contents, usually
#'   created from \code{\link[leafpop]{popupTable}}. See \code{\link{addControl}} for
#'   details.
#' @param label For vector data (sf/sp) a character vector of labels to be
#'   shown on mouseover. See \code{\link{addControl}} for details. For raster
#'   data (Raster*/stars) a logical indicating whether to add image query.
#' @param native.crs logical whether to reproject to web map coordinate
#'   reference system (web mercator - epsg:3857) or render using native CRS of
#'   the supplied data (can also be NA). Default is FALSE which will render in
#'   web mercator. If set to TRUE now background maps will be drawn (but rendering
#'   may be much quicker as no reprojecting is necessary). Currently only works
#'   for simple features.
#' @param method for raster data only (raster/stars). Method used to compute
#'   values for the resampled layer that is passed on to leaflet. mapview does
#'   projection on-the-fly to ensure correct display and therefore needs to know
#'   how to do this projection. The default is 'bilinear' (bilinear interpolation),
#'   which is appropriate for continuous variables. The other option, 'ngb'
#'   (nearest neighbor), is useful for categorical variables. Ignored if the raster
#'   layer is of class \code{factor} in which case "ngb" is used.
#' @param highlight either \code{FALSE}, \code{NULL} or a list of styling
#'   options for feature highlighting on mouse hover.
#'   See \code{\link{highlightOptions}} for details.
#' @param maxpoints the maximum number of points making up the geometry.
#'   In case of lines and polygons this refers to the number of vertices. See
#'   Details for more information.
#' @param query.type for raster methods only. Whether to show raster value query
#'   on \code{'mousemove'} or \code{'click'}. Ignored if \code{label = FALSE}.
#' @param query.digits for raster methods only. The amount of digits to be shown
#'   by raster value query. Ignored if \code{label = FALSE}.
#' @param query.position for raster methods only. The position of the raster
#'   value query info box. See \code{position} argument of \code{\link{addLegend}}
#'   for possible values. Ignored if \code{label = FALSE}.
#' @param query.prefix for raster methods only. a character string to be shown
#'   as prefix for the layerId. Ignored if \code{label = FALSE}.
#' @param ... additional arguments passed on to respective functions.
#'   See \code{\link{addRasterImage}}, \code{\link{addCircles}},
#' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
#'
#' @details
#' \code{maxpoints} is taken to determine when to switch rendering from svg
#'   to canvas overlay for perfomance. The threshold calculation is done as follows: \cr
#'   if the number of points (in case of point data) or vertices (in case of
#'   polygon or line data) > \code{maxpoints} then render using special render
#'   function. Within this render function we approximate the complexity of
#'   features by \cr
#' \cr
#' \code{maxFeatures <- maxfeatures / (npts(data) / length(data))} \cr
#' \cr
#'   where \code{npts} determines the number of points/vertices and \code{length}
#'   the number of features (points, lines or polygons). When the number of
#'   features in the current view window is larger than \code{maxFeatures} then
#'   features are rendered on the canvas, otherwise they are rendered as svg objects
#'   and fully queriable.
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' \dontrun{
#'   mapview()
#'
#'   ## simple features ====================================================
#'   library(sf)
#'
#'   # sf
#'   mapview(breweries)
#'   mapview(franconia)
#'
#'   # sfc
#'   mapview(st_geometry(breweries)) # no popup
#'
#'   # sfg / XY - taken from ?sf::st_point
#'   outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#'   hole1 = matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#'   hole2 = matrix(c(5,5,5,6,6,6,6,5,5,5),ncol=2, byrow=TRUE)
#'   pts = list(outer, hole1, hole2)
#'   (pl1 = st_polygon(pts))
#'   mapview(pl1)
#'
#'   ## raster ==============================================================
#'   if (interactive()) {
#'     library(plainview)
#'
#'     mapview(plainview::poppendorf[[5]])
#'   }
#'
#'   ## spatial objects =====================================================
#'   mapview(leaflet::gadmCHE)
#'   mapview(leaflet::atlStorms2005)
#'
#'
#'   ## styling options & legends ===========================================
#'   mapview(franconia, color = "white", col.regions = "red")
#'   mapview(franconia, color = "magenta", col.regions = "white")
#'
#'   mapview(breweries, zcol = "founded")
#'   mapview(breweries, zcol = "founded", at = seq(1400, 2200, 200), legend = TRUE)
#'   mapview(franconia, zcol = "district", legend = TRUE)
#'
#'   clrs <- sf.colors
#'   mapview(franconia, zcol = "district", col.regions = clrs, legend = TRUE)
#'
#'   ### multiple layers ====================================================
#'   mapview(franconia) + breweries
#'   mapview(list(breweries, franconia))
#'   mapview(franconia) + mapview(breweries) + trails
#'
#'   mapview(franconia, zcol = "district") + mapview(breweries, zcol = "village")
#'   mapview(list(franconia, breweries),
#'           zcol = list("district", NULL),
#'           legend = list(TRUE, FALSE))
#'
#'
#'   ### burst ==============================================================
#'   mapview(franconia, burst = TRUE)
#'   mapview(franconia, burst = TRUE, hide = TRUE)
#'   mapview(franconia, zcol = "district", burst = TRUE)
#'
#'
#'   ### ceci constitue la fin du pipe ======================================
#'   library(dplyr)
#'   library(sf)
#'
#'   franconia %>%
#'     sf::st_union() %>%
#'     mapview()
#'
#'   franconia %>%
#'     group_by(district) %>%
#'     summarize() %>%
#'     mapview(zcol = "district")
#'
#'   franconia %>%
#'     group_by(district) %>%
#'     summarize() %>%
#'     mutate(area = st_area(.) / 1e6) %>%
#'     mapview(zcol = "area")
#'
#'   franconia %>%
#'     mutate(area = sf::st_area(.)) %>%
#'     mapview(zcol = "area", legend = TRUE)
#'
#'   breweries %>%
#'     st_intersection(franconia) %>%
#'     mapview(zcol = "district")
#'
#'   franconia %>%
#'     mutate(count = lengths(st_contains(., breweries))) %>%
#'     mapview(zcol = "count")
#'
#'   franconia %>%
#'     mutate(count = lengths(st_contains(., breweries)),
#'            density = count / st_area(.)) %>%
#'     mapview(zcol = "density")
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
                   col.regions = mapviewGetOption("raster.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   use.layer.names = mapviewGetOption("use.layer.names"),
                   map.types = mapviewGetOption("basemaps"),
                   alpha.regions = 0.8,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   trim = mapviewGetOption("trim"),
                   verbose = mapviewGetOption("verbose"),
                   layer.name = NULL,
                   homebutton = mapviewGetOption("homebutton"),
                   native.crs = mapviewGetOption("native.crs"),
                   method = mapviewGetOption("method"),
                   label = TRUE,
                   query.type = mapviewGetOption("query.type"),
                   query.digits = mapviewGetOption("query.digits"),
                   query.position = mapviewGetOption("query.position"),
                   query.prefix = mapviewGetOption("query.prefix"),
                   viewer.suppress = mapviewGetOption("viewer.suppress"),
                   ...) {

            if (is.null(at)) at <- lattice::do.breaks(
              extendLimits(range(x[], na.rm = TRUE)), 256)

            if (mapviewGetOption("platform") != "leaflet") {
              warning(
                sprintf(
                  "platform '%s' currently doesn't support raster data."
                  , mapviewGetOption("platform")
                )
                , " switching to platform 'leaflet'"
                , call. = FALSE
              )
              mapviewOptions(platform = "leaflet")
            }

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
                          na.color = na.color,
                          use.layer.names = use.layer.names,
                          map.types = map.types,
                          alpha.regions = alpha.regions,
                          legend = legend,
                          legend.opacity = legend.opacity,
                          trim = trim,
                          verbose = verbose,
                          layer.name = layer.name,
                          homebutton = homebutton,
                          native.crs = native.crs,
                          method = method,
                          label = label,
                          query.type = query.type,
                          query.digits = query.digits,
                          query.position = query.position,
                          query.prefix = query.prefix,
                          viewer.suppress = viewer.suppress,
                          ...)
            } else {
              NULL
            }

          }

)



## Stars layer ==================================================================
#' @describeIn mapView \code{\link{stars}}
setMethod('mapView', signature(x = 'stars'),
          function(x,
                   band = 1,
                   map = NULL,
                   maxpixels = mapviewGetOption("mapview.maxpixels"),
                   col.regions = mapviewGetOption("raster.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   use.layer.names = mapviewGetOption("use.layer.names"),
                   map.types = mapviewGetOption("basemaps"),
                   alpha.regions = 0.8,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   trim = mapviewGetOption("trim"),
                   verbose = mapviewGetOption("verbose"),
                   layer.name = NULL,
                   homebutton = mapviewGetOption("homebutton"),
                   native.crs = mapviewGetOption("native.crs"),
                   method = mapviewGetOption("method"),
                   label = TRUE,
                   query.type = mapviewGetOption("query.type"),
                   query.digits = mapviewGetOption("query.digits"),
                   query.position = mapviewGetOption("query.position"),
                   query.prefix = mapviewGetOption("query.prefix"),
                   viewer.suppress = mapviewGetOption("viewer.suppress"),
                   ...) {

            # method = match.arg(method)
            if(length(dim(x)) == 2) layer = x[[1]] else layer = x[[1]][, , band]
			# EJP: handle factors first
            if (is.null(at)) {
                at = if (is.factor(x[[1]]))
                        as.vector(layer)
                     else
                        lattice::do.breaks(extendLimits(range(layer, na.rm = TRUE)), 256)
            }

            if (mapviewGetOption("platform") != "leaflet") {
              warning(
                sprintf(
                  "platform '%s' currently doesn't support stars data."
                  , mapviewGetOption("platform")
                )
                , " switching to platform 'leaflet'"
                , call. = FALSE
              )
              mapviewOptions(platform = "leaflet")
            }

            if (mapviewGetOption("platform") == "leaflet") {
              leaflet_stars(x,
                            band = band,
                            map = map,
                            maxpixels = maxpixels,
                            col.regions = col.regions,
                            at = at,
                            na.color = na.color,
                            use.layer.names = use.layer.names,
                            map.types = map.types,
                            alpha.regions = alpha.regions,
                            legend = legend,
                            legend.opacity = legend.opacity,
                            trim = trim,
                            verbose = verbose,
                            layer.name = layer.name,
                            homebutton = homebutton,
                            native.crs = native.crs,
                            method = method,
                            label = label,
                            query.type = query.type,
                            query.digits = query.digits,
                            query.position = query.position,
                            query.prefix = query.prefix,
                            viewer.suppress = viewer.suppress,
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
                   col.regions = mapviewGetOption("raster.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   use.layer.names = TRUE,
                   map.types = mapviewGetOption("basemaps"),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = mapviewGetOption("verbose"),
                   homebutton = TRUE,
                   method = mapviewGetOption("method"),
                   label = TRUE,
                   query.type = c("mousemove", "click"),
                   query.digits,
                   query.position = mapviewGetOption("query.position"),
                   query.prefix = "Layer",
                   viewer.suppress = mapviewGetOption("viewer.suppress"),
                   ...) {

            if (mapviewGetOption("platform") != "leaflet") {
              warning(
                sprintf(
                  "platform '%s' currently doesn't support raster data."
                  , mapviewGetOption("platform")
                )
                , " switching to platform 'leaflet'"
                , call. = FALSE
              )
              mapviewOptions(platform = "leaflet")
            }

            if (mapviewGetOption("platform") == "leaflet") {
              leafletRSB(x,
                         map = map,
                         maxpixels = maxpixels,
                         col.regions = col.regions,
                         at = at,
                         na.color = na.color,
                         use.layer.names = use.layer.names,
                         map.types = map.types,
                         legend = legend,
                         legend.opacity = legend.opacity,
                         trim = trim,
                         verbose = verbose,
                         homebutton = homebutton,
                         method = method,
                         label = label,
                         query.type = query.type,
                         query.digits = query.digits,
                         query.position = query.position,
                         query.prefix = query.prefix,
                         viewer.suppress = viewer.suppress,
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
                   col.regions = mapviewGetOption("raster.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   map.types = mapviewGetOption("basemaps"),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = mapviewGetOption("verbose"),
                   homebutton = TRUE,
                   method = c("bilinear", "ngb"),
                   label = TRUE,
                   ...) {

            if (mapviewGetOption("platform") != "leaflet") {
              warning(
                sprintf(
                  "platform '%s' currently doesn't support Satellite data."
                  , mapviewGetOption("platform")
                )
                , " switching to platform 'leaflet'"
                , call. = FALSE
              )
              mapviewOptions(platform = "leaflet")
            }

            if (mapviewGetOption("platform") == "leaflet") {
              leafletSatellite(x,
                               map = map,
                               maxpixels = maxpixels,
                               col.regions = col.regions,
                               at = at,
                               na.color = na.color,
                               map.types = map.types,
                               legend = legend,
                               legend.opacity = legend.opacity,
                               trim = trim,
                               verbose = verbose,
                               homebutton = homebutton,
                               method = method,
                               label = label,
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
                   pane = "auto",
                   canvas = useCanvas(x),
                   viewer.suppress = canvas,
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
                   na.alpha = regionOpacity(x),
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   popup = leafpop::popupTable(x),
                   layer.name = NULL,
                   label = makeLabels(x, zcol),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   highlight = mapviewHighlightOptions(x, alpha.regions, alpha, lwd),
                   maxpoints = getMaxFeatures(x),
                   ...) {

            if (nrow(x) == 0) {
              stop("\n", deparse(substitute(x, env = parent.frame())),
                   " does not contain data \n", call. = FALSE)
            }

            if (length(unique(sf::st_dimension(x))) > 1) {
              x = sf::st_cast(x)
            }

            if (is.null(zcol) & is.null(legend)) legend = FALSE
            if (!is.null(zcol) && !all(zcol %in% colnames(x))) {
              stop("\n", "at least one of the following columns: \n",
                   "'", paste(zcol, collapse = "', '"), "' \nnot found in ",
                   deparse(substitute(x, env = parent.frame())),
                   call. = FALSE)
            }

            ## do we burst?
            # zcol = unique(unlist(zcol))

            if (is.character(burst)) {
              zcol = burst
              burst = TRUE
            }

            if (length(zcol) > 1) {
              x = x[, zcol]
              burst = TRUE
            }

            if (burst) {
              by_row = FALSE
              # special treatment for row based bursting
              if (!is.null(zcol) && burst && length(zcol) == 1) {
                color = vectorColors(x = x,
                                     zcol = zcol,
                                     colors = color,
                                     at = at,
                                     na.color = na.color)
                if (length(color) > 1)
                  color = color[order(x[[zcol]])]
                col.regions = vectorColRegions(x = x,
                                               zcol = zcol,
                                               col.regions = col.regions,
                                               at = at,
                                               na.color = na.color)
                if (length(col.regions) > 1)
                  col.regions = col.regions[order(x[[zcol]])]

                popup = leafpop::popupTable(x)[order(x[[zcol]])]
                label = makeLabels(x, zcol)[order(x[[zcol]])]
                by_row = TRUE
              }

              # for whatever reason we need to evaluate a few things here...??
              popup = popup
              lwd = lineWidth(x)
              alpha.regions = regionOpacity(x)
              x <- burst(x = x,
                         zcol = zcol,
                         burst = burst)
            }

            if (inherits(x, "list")) {
              mapView(x,
                      map = map,
                      zcol = zcol,
                      burst = FALSE,
                      color = color,
                      col.regions = col.regions,
                      popup = popup,
                      label = label,
                      homebutton = homebutton,
                      legend = legend,
                      map.types = map.types,
                      layer.name = layer.name,
                      alpha = alpha,
                      alpha.regions = alpha.regions,
                      na.alpha = na.alpha,
                      canvas = canvas,
                      viewer.suppress = viewer.suppress,
                      pane = pane,
                      cex = cex,
                      lwd = lwd,
                      by_row = by_row,
                      ...)
            } else if (mapviewGetOption("platform") == "leaflet") {

                leaflet_sf(x,
                           map = map,
                           pane = pane,
                           zcol = zcol,
                           color = color,
                           col.regions = col.regions,
                           at = at,
                           na.color = na.color,
                           cex = cex,
                           lwd = lwd,
                           alpha = alpha,
                           alpha.regions = alpha.regions,
                           na.alpha = na.alpha,
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
                           canvas = canvas,
                           viewer.suppress = viewer.suppress,
                           ...)

            } else if (mapviewGetOption("platform") == "leafgl") {

              leafgl_sf(x,
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
                        na.alpha = na.alpha,
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
                        viewer.suppress = viewer.suppress,
                        ...)

            } else if (mapviewGetOption("platform") == "mapdeck") {

              mapdeck_sf(x,
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
                         na.alpha = na.alpha,
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
                         viewer.suppress = viewer.suppress,
                         ...)

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
                   pane = "auto",
                   canvas = useCanvas(x),
                   viewer.suppress = canvas,
                   color = standardColor(x), #mapviewGetOption("vector.palette"),
                   col.regions = standardColRegions(x), #mapviewGetOption("vector.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   cex = 6,
                   lwd = lineWidth(x),
                   alpha = 0.9,
                   alpha.regions = regionOpacity(x),
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   popup = NULL,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label = makeLabels(x),
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   highlight = mapviewHighlightOptions(x, alpha.regions, alpha, lwd),
                   maxpoints = getMaxFeatures(x),
                   ...) {

            if (length(x) == 0) {
              stop("\n", deparse(substitute(x, env = parent.frame())),
                   " does not contain data \n", call. = FALSE)
            }

            if (length(unique(sf::st_dimension(x))) > 1) {
              x = sf::st_cast(x)
            }

            if (mapviewGetOption("platform") == "leaflet") {

              leaflet_sfc(x,
                          map = map,
                          pane = pane,
                          canvas = canvas,
                          viewer.suppress = viewer.suppress,
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

            } else if (mapviewGetOption("platform") == "leafgl") {

              leafgl_sfc(x,
                         map = map,
                         pane = pane,
                         canvas = canvas,
                         viewer.suppress = viewer.suppress,
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

            } else if (mapviewGetOption("platform") == "mapdeck") {

              mapdeck_sfc(x,
                          map = map,
                          pane = pane,
                          canvas = canvas,
                          viewer.suppress = viewer.suppress,
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


## character ==============================================================
#' @param tms whether the tiles are served as TMS tiles.
#'
#' @describeIn mapView \code{\link{character}}
setMethod('mapView', signature(x = 'character'),
         function(x,
                  map = NULL,
                  tms = TRUE,
                  color = standardColor(),
                  col.regions = standardColRegions(),
                  at = NULL,
                  na.color = mapviewGetOption("na.color"),
                  cex = 6,
                  lwd = 2,
                  alpha = 0.9,
                  alpha.regions = 0.6,
                  na.alpha = 0.6,
                  map.types = mapviewGetOption("basemaps"),
                  verbose = FALSE,
                  layer.name = x,
                  homebutton = TRUE,
                  native.crs = FALSE,
                  canvas = FALSE,
                  viewer.suppress = mapviewGetOption("viewer.suppress"),
                  ...) {
           if (mapviewGetOption("platform") == "leaflet") {
             if (utils::file_test("-d", x)) {
               leaflet_tiles(x = x,
                             map = map,
                             tms = tms,
                             map.types = map.types,
                             verbose = verbose,
                             layer.name = layer.name,
                             homebutton = homebutton,
                             native.crs = native.crs,
                             viewer.suppress = viewer.suppress,
                             ...)
             } else if (utils::file_test("-f", x)) {
               layer.name = basename(tools::file_path_sans_ext(layer.name))
               leaflet_file(x = x,
                            map = map,
                            color = color,
                            col.regions = col.regions,
                            at = at,
                            na.color = na.color,
                            cex = cex,
                            lwd = lwd,
                            alpha = alpha,
                            alpha.regions = alpha.regions,
                            na.alpha = na.alpha,
                            map.types = map.types,
                            verbose = verbose,
                            layer.name = layer.name,
                            homebutton = homebutton,
                            native.crs = native.crs,
                            canvas = canvas,
                            viewer.suppress = viewer.suppress,
                            ...)
             } else {
               stop(sprintf("%s is not a directory!", layer.name),
                    call. = FALSE)
             }
           }
         }
)

## numeric ================================================================
#' @describeIn mapView \code{\link{numeric}}
#' @param y numeric vector.
#' @param type whether to render the numeric vector \code{x} as a
#' point \code{"p"} or line \code{"l"} plot.
setMethod('mapView', signature(x = 'numeric'),
          function(x, y, type = "p", grid = TRUE, label, ...) {
            if (missing(label)) {
              if (!missing(y)) {
                labs = lapply(seq(length(x)), function(i) {
                  paste0("x : ", x[i], '<br>',
                         "y : ", y[i])
                })
                label = lapply(labs, htmltools::HTML)
              } else {
                labs = lapply(seq(length(x)), function(i) {
                  paste0("x : ", seq_along(x)[i], '<br>',
                         "y : ", x[i])
                })
                label = lapply(labs, htmltools::HTML)
              }
            }
            if (type == "l") label = NULL
            xyView(x = x,
                   y = y,
                   type = type,
                   grid = grid,
                   label = label,
                   ...)
          }
)


## data.frame =============================================================
#' @describeIn mapView \code{\link{data.frame}}
#' @param xcol the column to be mapped to the x-axis. Only relevant for the
#' data.frame method.
#' @param ycol the column to be mapped to the y-axis. Only relevant for the
#' data.frame method.
#' @param grid whether to plot a (scatter plot) xy-grid to aid interpretation
#' of the visualisation. Only relevant for the data.frame method.
#' @param aspect the ratio of x/y axis corrdinates to adjust the plotting
#' space to fit the screen. Only relevant for the data.frame method.
#' @param crs an optional crs specification for the provided data to enable
#' rendering on a basemap. See argument description in \code{\link{st_sf}}
#' for details.
setMethod('mapView', signature(x = 'data.frame'),
          function(x,
                   xcol,
                   ycol,
                   grid = TRUE,
                   aspect = 1,
                   popup = leafpop::popupTable(x),
                   label,
                   crs = NA,
                   ...) {
            if (missing(xcol) | missing(ycol)) {
              obj = deparse(substitute(x, env = parent.frame()))
              msg = paste0("\noops! Arguments xcol and/or ycol are missing!\n",
                           "You probably expected ", obj,
                           " to be a spatial object. \nHowever it is of class ",
                           class(x), ". \nEither convert ", obj, " to a spatial object ",
                           "or provide xcol and ycol.")
              stop(msg, call. = FALSE)
            }
            if (missing(label)) {
              labs = lapply(seq(nrow(x)), function(i) {
                paste0(xcol, " (x) : ", x[[xcol]][i], '<br>',
                       ycol, " (y) : ", x[[ycol]][i])
              })
              label = lapply(labs, htmltools::HTML)
            }
            xyView(x = xcol,
                   y = ycol,
                   data = x,
                   grid = grid,
                   aspect = aspect,
                   popup = popup,
                   label = label,
                   crs = crs,
                   ...)
          }
)



## XY =====================================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'XY'),
          function(x,
                   map = NULL,
                   pane = "auto",
                   canvas = useCanvas(x),
                   viewer.suppress = mapviewGetOption("viewer.suppress"),
                   color = standardColor(x), #mapviewGetOption("vector.palette"),
                   col.regions = standardColRegions(x), #mapviewGetOption("vector.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   cex = 6,
                   lwd = lineWidth(x),
                   alpha = 0.9,
                   alpha.regions = regionOpacity(x),
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
                   highlight = mapviewHighlightOptions(x, alpha.regions, alpha, lwd),
                   maxpoints = getMaxFeatures(x),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {

              x = sf::st_cast(sf::st_sfc(x))
              leaflet_sfc(x,
                          map = map,
                          pane = pane,
                          canvas = canvas,
                          viewer.suppress = viewer.suppress,
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


## XYZ ====================================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'XYZ'),
          function(x,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame(1))),
                   ...) {
            mapview(sf::st_zm(x), layer.name = layer.name, ...)
          }
)



## XYM ====================================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'XYM'),
          function(x,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame(1))),
                   ...) {
            mapview(sf::st_zm(x), layer.name = layer.name, ...)
          }
)



## XYZM ===================================================================
#' @describeIn mapView \code{\link{st_sfc}}

setMethod('mapView', signature(x = 'XYZM'),
          function(x,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame(1))),
                   ...) {
            mapview(sf::st_zm(x), layer.name = layer.name, ...)
          }
)



## bbox =======================================================
#' @describeIn mapView \code{\link{st_bbox}}

setMethod('mapView', signature(x = 'bbox'),
          function(x,
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame(1))),
                   alpha.regions = 0.2,
                   ...) {
            mapview(sf::st_as_sfc(x),
                    layer.name = layer.name,
                    alpha.regions = alpha.regions,
                    ...)
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

## NULL ===================================================================
#' @describeIn mapView initiate a map without an object
#'
setMethod('mapView', signature(x = 'NULL'),
          function(x, ...) {
              NULL
          }
)



## list ===================================================================
#' @describeIn mapView \code{\link{list}}
#'
setMethod('mapView', signature(x = 'list'),
          function(x,
                   map = NULL,
                   zcol = NULL,
                   burst = FALSE,
                   color = mapviewGetOption("vector.palette"),
                   col.regions = mapviewGetOption("vector.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   cex = 6,
                   lwd = lapply(x, lineWidth),
                   alpha = 0.9,
                   alpha.regions = lapply(x, regionOpacity),
                   na.alpha = lapply(x, regionOpacity),
                   map.types = mapviewGetOption("basemaps"),
                   verbose = mapviewGetOption("verbose"),
                   popup = lapply(x, leafpop::popupTable),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   label = lapply(x, makeLabels),
                   legend = mapviewGetOption("legend"),
                   homebutton = TRUE,
                   native.crs = FALSE,
                   ...) {

            if ("by_row" %in% names(list(...))) {
              listify = listifyer(x, by_row = list(...)$by_row)
            } else {
              listify = listifyer(x)
            }

            lyrnms = makeListLayerNames(x, layer.name)

            m <- Reduce("+", lapply(seq(x), function(i) {
              mapView(x = x[[i]],
                      map = map,
                      color = listify(color)[[i]],
                      col.regions = listify(col.regions)[[i]],
                      at = listify(at)[[i]],
                      na.color = listify(na.color)[[i]],
                      cex = listify(cex)[[i]],
                      lwd = listify(lwd)[[i]],
                      alpha = listify(alpha)[[i]],
                      alpha.regions = listify(alpha.regions)[[i]],
                      map.types = map.types,
                      verbose = verbose,
                      popup = listify(popup)[[i]],
                      layer.name = lyrnms[[i]],
                      label = listify(label)[[i]],
                      legend = listify(legend)[[i]],
                      homebutton = listify(homebutton)[[i]],
                      native.crs = native.crs,
                      ...)
            }))@map

            out <- new("mapview", object = x, map = m)
            return(out)
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
                   map = NULL,
                   zcol = NULL,
                   maxpixels = mapviewGetOption("mapview.maxpixels"),
                   col.regions = mapviewGetOption("raster.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   use.layer.names = FALSE,
                   map.types = mapviewGetOption("basemaps"),
                   alpha.regions = 0.8,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = mapviewGetOption("verbose"),
                   layer.name = NULL,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   method = mapviewGetOption("method"),
                   label = TRUE,
                   query.type = c("mousemove", "click"),
                   query.digits,
                   query.position = "topright",
                   query.prefix = "Layer",
                   viewer.suppress = mapviewGetOption("viewer.suppress"),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {
              leafletPixelsDF(x = x,
                              map = map,
                              zcol = zcol,
                              maxpixels = maxpixels,
                              col.regions = col.regions,
                              at = at,
                              na.color = na.color,
                              use.layer.names = use.layer.names,
                              map.types = map.types,
                              alpha.regions = alpha.regions,
                              legend = legend,
                              legend.opacity = legend.opacity,
                              trim = trim,
                              verbose = verbose,
                              layer.name = layer.name,
                              homebutton = homebutton,
                              native.crs = native.crs,
                              method = method,
                              label = label,
                              query.type = query.type,
                              query.digits = query.digits,
                              query.position = query.position,
                              query.prefix = query.prefix,
                              viewer.suppress = viewer.suppress,
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
                   map = NULL,
                   zcol = NULL,
                   maxpixels = mapviewGetOption("mapview.maxpixels"),
                   col.regions = mapviewGetOption("raster.palette"),
                   at = NULL,
                   na.color = mapviewGetOption("na.color"),
                   use.layer.names = FALSE,
                   map.types = mapviewGetOption("basemaps"),
                   alpha.regions = 0.8,
                   legend = mapviewGetOption("legend"),
                   legend.opacity = 1,
                   trim = TRUE,
                   verbose = mapviewGetOption("verbose"),
                   layer.name = NULL,
                   homebutton = TRUE,
                   native.crs = FALSE,
                   method = mapviewGetOption("method"),
                   label = TRUE,
                   query.type = c("mousemove", "click"),
                   query.digits,
                   query.position = "topright",
                   query.prefix = "Layer",
                   viewer.suppress = mapviewGetOption("viewer.suppress"),
                   ...) {

            if (mapviewGetOption("platform") == "leaflet") {
              leafletPixelsDF(x = as(x, "SpatialPixelsDataFrame"),
                              map = map,
                              zcol = zcol,
                              maxpixels = maxpixels,
                              col.regions = col.regions,
                              at = at,
                              na.color = na.color,
                              use.layer.names = use.layer.names,
                              map.types = map.types,
                              alpha.regions = alpha.regions,
                              legend = legend,
                              legend.opacity = legend.opacity,
                              trim = trim,
                              verbose = verbose,
                              layer.name = layer.name,
                              homebutton = homebutton,
                              native.crs = native.crs,
                              method = method,
                              label = label,
                              query.type = query.type,
                              query.digits = query.digits,
                              query.position = query.position,
                              query.prefix = query.prefix,
                              viewer.suppress = viewer.suppress,
                              ...)
            } else {
              NULL
            }

          }
)


## SpatialPointsDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialPointsDataFrame}}
setMethod('mapView', signature(x = 'SpatialPointsDataFrame'),
          function(x,
                   zcol = NULL,
                   layer.name = NULL,
                   ...) {
            if (is.null(layer.name))
              layer.name = makeLayerName(x, zcol, up = 2)
            mapView(st_as_sf(x), layer.name = layer.name, zcol = zcol, ...)
          }
)


## SpatialPoints ==========================================================
#' @describeIn mapView \code{\link{SpatialPoints}}

setMethod('mapView', signature(x = 'SpatialPoints'),
          function(x,
                   zcol = NULL,
                   layer.name = NULL,
                   ...) {
            if (is.null(layer.name))
              layer.name = makeLayerName(x, zcol, up = 2)
            mapView(st_as_sfc(x), layer.name = layer.name, zcol = zcol, ...)
          }
)


## SpatialPolygonsDataFrame ===============================================
#' @describeIn mapView \code{\link{SpatialPolygonsDataFrame}}

setMethod('mapView', signature(x = 'SpatialPolygonsDataFrame'),
          function(x,
                   zcol = NULL,
                   layer.name = NULL,
                   ...) {
            if (is.null(layer.name))
              layer.name = makeLayerName(x, zcol, up = 2)
            mapView(st_as_sf(x), layer.name = layer.name, zcol = zcol, ...)
          }
)


## SpatialPolygons ========================================================
#' @describeIn mapView \code{\link{SpatialPolygons}}

setMethod('mapView', signature(x = 'SpatialPolygons'),
          function(x,
                   zcol = NULL,
                   layer.name = NULL,
                   ...) {
            if (is.null(layer.name))
              layer.name = makeLayerName(x, zcol, up = 2)
            mapView(st_as_sfc(x), layer.name = layer.name, zcol = zcol, ...)
          }
)


## SpatialLinesDataFrame =================================================
#' @describeIn mapView \code{\link{SpatialLinesDataFrame}}

setMethod('mapView', signature(x = 'SpatialLinesDataFrame'),
          function(x,
                   zcol = NULL,
                   layer.name = NULL,
                   ...) {
            if (is.null(layer.name))
              layer.name = makeLayerName(x, zcol, up = 2)
            mapView(st_as_sf(x), layer.name = layer.name, zcol = zcol, ...)
          }
)


## SpatialLines ===========================================================
#' @describeIn mapView \code{\link{SpatialLines}}

setMethod('mapView', signature(x = 'SpatialLines'),
          function(x,
                   zcol = NULL,
                   layer.name = NULL,
                   ...) {
            if (is.null(layer.name))
              layer.name = makeLayerName(x, zcol, up = 2)
            mapView(st_as_sfc(x), layer.name = layer.name, zcol = zcol, ...)
          }
)




# legacy -----
# ## sfc_POINT
# #' @describeIn mapView \code{\link{st_sfc}}
#
# setMethod('mapView', signature(x = 'sfc_POINT'),
#           function(x, ...) {
#             callNextMethod()
#           }
# )
#
#
# ## sfc_MULTIPOINT
# #' @describeIn mapView \code{\link{st_sfc}}
#
# setMethod('mapView', signature(x = 'sfc_MULTIPOINT'),
#           function(x, ...) {
#             callNextMethod()
#           }
# )
#
#
# ## sfc_LINESTRING
# #' @describeIn mapView \code{\link{st_sfc}}
#
# setMethod('mapView', signature(x = 'sfc_LINESTRING'),
#           function(x, ...) {
#             callNextMethod()
#           }
# )
#
#
# ## sfc_MULTILINESTRING
# #' @describeIn mapView \code{\link{st_sfc}}
#
# setMethod('mapView', signature(x = 'sfc_MULTILINESTRING'),
#           function(x, ...) {
#             callNextMethod()
#           }
# )
#
#
# ## sfc_POLYGON
# #' @describeIn mapView \code{\link{st_sfc}}
#
# setMethod('mapView', signature(x = 'sfc_POLYGON'),
#           function(x, ...) {
#             callNextMethod()
#           }
# )
#
#
# ## sfc_MULTIPOLYGON
# #' @describeIn mapView \code{\link{st_sfc}}
#
# setMethod('mapView', signature(x = 'sfc_MULTIPOLYGON'),
#           function(x, ...) {
#             callNextMethod()
#           }
# )
#
#
# ## sfc_GEOMETRY
# #' @describeIn mapView \code{\link{st_sfc}}
#
# setMethod('mapView', signature(x = 'sfc_GEOMETRY'),
#           function(x, ...) {
#             callNextMethod()
#           }
# )
