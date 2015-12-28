if ( !isGeneric('plainView') ) {
  setGeneric('plainView', function(x, ...)
    standardGeneric('plainView'))
}

#' View raster objects interactively without background map but in any CRS
#'
#' @description
#' this function produces an interactive view of the specified
#' raster object(s) on a plain grey background but for any CRS.
#'
#' @param x a \code{\link{raster}}* object
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param col.regions color (palette).See \code{\link{levelplot}} for details.
#' @param at the breakpoints used for the visualisation. See
#' \code{\link{levelplot}} for details.
#' @param na.color color for missing values
#' @param verbose should some details be printed during the process
#' @param layer.name the name of the layer to be shown on the map
#' @param gdal logical. If TRUE (default) gdalUtils::gdal_translate is used
#' to create the png file for display when possible. See details for further
#' information.
#' @param ... additional arguments passed on to repective functions.
#' See \code{\link{addRasterImage}}, \code{\link{addCircles}},
#' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
#'
#' @details
#' if the raster object is not in memory
#' (i.e. if \code{raster::inMemory} is \code{FLASE})
#' and argument \code{gdal} is set to TRUE (default) gdalUtils::gdal_translate
#' is used to translate the rsater object to a png file to be rendered in
#' the viewer/browser. This is fast for large rasters.
#' However, this means that RasterLayers will be shown in greyscale.
#' If you want to set a color palette manually, use \code{gdal = FALSE} and
#' (optionally provide) \code{col.regions}.
#'
#' @author
#' Tim Appelhans
#' @author
#' Stephan Woellauer
#'
#' @examples
#' \dontrun{
#' plainView()
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
#' m1 <- plainView(poppendorf[[10]])
#' m1
#'
#' # SpatialPixelsDataFrame
#' plainView(meuse.grid, zcol = "soil")
#'
#'
#' ### point vector data ###
#' ## SpatialPointsDataFrame ##
#' data(meuse)
#' coordinates(meuse) <- ~x+y
#' proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#' # all layers of meuse
#' plainView(meuse, burst = TRUE)
#'
#' # only one layer, all info in popups
#' plainView(meuse)
#'
#' ## SpatialPoints ##
#' meuse_pts <- as(meuse, "SpatialPoints")
#' plainView(meuse_pts)
#'
#'
#'
#' ### overlay vector on top of raster ###
#' plainView(meuse.grid, zcol = "ffreq") + meuse
#'
#' ### polygon vector data ###
#' data("gadmCHE")
#' m <- plainView(gadmCHE)
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
#' plainView(atlStorms2005)
#' plainView(atlStorms2005, burst = TRUE)
#' }
#'
#' @export plainView
#' @name plainView
#' @rdname plainView
#' @aliases plainView,RasterLayer-method

## RasterLayer ============================================================

setMethod('plainView', signature(x = 'RasterLayer'),
          function(x,
                   maxpixels = mapviewGetOption("maxpixels"),
                   col.regions = mapviewGetOption("raster.palette")(256),
                   at,
                   na.color = mapviewGetOption("na.color"),
                   verbose = mapviewGetOption("verbose"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   gdal = TRUE,
                   ...) {

            ## temp dir
            dir <- tempfile()
            dir.create(dir)
            fl <- paste0(dir, "/img", ".png")

            if (raster::fromDisk(x) & gdal) {
              gdalUtils::gdal_translate(src_dataset = filename(x),
                                        dst_dataset = fl,
                                        of = "PNG",
                                        b = bandnr(x),
                                        verbose = verbose)
            } else {
              png <- raster2PNG(x,
                                col.regions = col.regions,
                                at = at,
                                na.color = na.color,
                                maxpixels = maxpixels)

              png::writePNG(png, fl)
            }

            plainViewInternal(filename = fl,
                              imgnm = layer.name)

          }

)

# ## Raster Stack/Brick ===========================================================
# #' @describeIn plainView \code{\link{stack}} / \code{\link{brick}}
#
# setMethod('plainView', signature(x = 'RasterStackBrick'),
#           function(x,
#                    map = NULL,
#                    maxpixels = mapviewOptions(console = FALSE)$maxpixels,
#                    color = mapViewPalette(7),
#                    na.color = mapviewOptions(console = FALSE)$nacolor,
#                    values = NULL,
#                    legend = FALSE,
#                    legend.opacity = 1,
#                    trim = TRUE,
#                    verbose = mapviewOptions(console = FALSE)$verbose,
#                    ...) {
#
#             if (mapviewOptions(console = FALSE)$platform == "leaflet") {
#               leafletPlainRSB(x,
#                               map,
#                               maxpixels,
#                               color,
#                               na.color,
#                               values,
#                               legend,
#                               legend.opacity,
#                               trim,
#                               verbose,
#                               ...)
#             } else {
#               NULL
#             }
#
#           }
# )
#
#
#
# ## Satellite object =======================================================
# #' @describeIn plainView \code{\link{satellite}}
#
# setMethod('plainView', signature(x = 'Satellite'),
#           function(x,
#                    ...) {
#
#             pkgs <- c("leaflet", "satellite", "magrittr")
#             tst <- sapply(pkgs, "requireNamespace",
#                           quietly = TRUE, USE.NAMES = FALSE)
#
#             lyrs <- x@layers
#
#             m <- plainView(lyrs[[1]], ...)
#
#             if (length(lyrs) > 1) {
#               for (i in 2:length(lyrs)) {
#                 m <- plainView(lyrs[[i]], m, ...)
#               }
#             }
#
#             if (length(getLayerNamesFromMap(m)) > 1) {
#               m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
#             }
#
#             out <- new('mapview', object = list(x), map = m)
#
#             return(out)
#
#           }
#
# )
#
#
# ## SpatialPixelsDataFrame =================================================
# #' @describeIn plainView \code{\link{SpatialPixelsDataFrame}}
# #'
# setMethod('plainView', signature(x = 'SpatialPixelsDataFrame'),
#           function(x,
#                    zcol = NULL,
#                    ...) {
#
#             if (mapviewOptions(console = FALSE)$platform == "leaflet") {
#               leafletPlainPixelsDF(x,
#                                    zcol,
#                                    ...)
#             } else {
#               NULL
#             }
#
#           }
# )
#
#
# #' <Add Title>
# #'
# #' <Add Description>
# #'
# #' @import htmlwidgets
# #'
# #' @export

plainViewInternal <- function(filename, imgnm) {

  x <- list(imgnm = imgnm)

  image_dir <- dirname(filename)
  image_file <- basename(filename)

  dep1 <- htmltools::htmlDependency(name = "image",
                                    version = "1",
                                    src = c(file = image_dir),
                                    attachment = list(image_file))
  deps <- list(dep1)

  sizing <- htmlwidgets::sizingPolicy(padding = 0, browser.fill = TRUE)

  htmlwidgets::createWidget(
    name = 'plainView',
    x = x,
    package = 'mapview',
    dependencies = deps,
    sizingPolicy = sizing
  )
}


plainViewOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'plainView',
                                 width, height, package = 'mapview')
}


renderPlainView <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, plainViewOutput, env, quoted = TRUE)
}



## Raster Stack/Brick ===========================================================
#' @describeIn plainView \code{\link{stack}} / \code{\link{brick}}
#'
#' @param r integer. Index of the Red channel, between 1 and nlayers(x)
#' @param g integer. Index of the Green channel, between 1 and nlayers(x)
#' @param b integer. Index of the Blue channel, between 1 and nlayers(x)

setMethod('plainView', signature(x = 'RasterStackBrick'),
          function(x, r = 3, g = 2, b = 1,
                   na.color = mapviewGetOption("na.color"),
                   maxpixels = mapviewGetOption("maxpixels"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            ## temp dir
            dir <- tempfile()
            dir.create(dir)
            fl <- paste0(dir, "/img", ".png")

            #             if (raster::filename(x) != "") {
            #               gdalUtils::gdal_translate(src_dataset = filename(x),
            #                                         dst_dataset = fl,
            #                                         of = "PNG",
            #                                         verbose = TRUE)
            #             } else {
            png <- rgbStack2PNG(x, r = r, g = g, b = b,
                                na.color = na.color,
                                maxpixels = maxpixels,
                                ...)
            png::writePNG(png, fl)
            #}

            layer.name <- paste0(layer.name, "_", r, ".", g, ".", b)
            plainViewInternal(filename = fl,
                              imgnm = layer.name)

          }

)



## SpatialPixelsDataFrame =================================================
#' @describeIn plainView \code{\link{SpatialPixelsDataFrame}}
#'
#' @param zcol attribute name or column number in attribute table
#' of the column to be rendered
#'
setMethod('plainView', signature(x = 'SpatialPixelsDataFrame'),
          function(x,
                   zcol = 1,
                   ...) {

            if (is.character(zcol)) nm <- zcol else  nm <- names(x)[zcol]
            x <- raster(x[zcol])

            plainView(x, layer.name = nm, ...)

          }
)







## plainview ==============================================================

if ( !isGeneric('plainview') ) {
  setGeneric('plainview', function(...)
    standardGeneric('plainview'))
}

#' @describeIn plainView alias for ease of typing
#' @aliases plainview
#' @export plainview

setMethod('plainview', signature('ANY'),
          function(...) plainView(...))











#
#
# ## Satellite object =======================================================
# #' @describeIn plainView \code{\link{satellite}}
#
# setMethod('plainView', signature(x = 'Satellite'),
#           function(x,
#                    ...) {
#
#             pkgs <- c("leaflet", "satellite", "magrittr")
#             tst <- sapply(pkgs, "requireNamespace",
#                           quietly = TRUE, USE.NAMES = FALSE)
#
#             lyrs <- x@layers
#
#             m <- plainView(lyrs[[1]], ...)
#
#             if (length(lyrs) > 1) {
#               for (i in 2:length(lyrs)) {
#                 m <- plainView(lyrs[[i]], m, ...)
#               }
#             }
#
#             if (length(getLayerNamesFromMap(m)) > 1) {
#               m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
#             }
#
#             out <- new('mapview', object = list(x), map = m)
#
#             return(out)
#
#           }
#
# )
#
#


