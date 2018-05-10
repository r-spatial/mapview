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
#' @param na.color color for missing values.
#' @param legend logical, whether to draw a legend for the raster layer.
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
#' If the raster object is not in memory
#' (i.e. if \code{raster::inMemory} is \code{FLASE})
#' and argument \code{gdal} is set to TRUE (default) gdalUtils::gdal_translate
#' is used to translate the rsater object to a png file to be rendered in
#' the viewer/browser. This is fast for large rasters. In this case, argument
#' \code{maxpixels} is not used, instead the image is rendered in original resolution.
#' However, this means that RasterLayers will be shown in greyscale.
#' If you want to set a color palette manually, use \code{gdal = FALSE} and
#' (optionally provide) \code{col.regions}.\cr
#' \cr
#' For plainView there are a few keyboard shortcuts defined:
#' \itemize{
#'   \item plus/minus - zoom in/out
#'   \item space - toggle antialiasing
#'   \item esc - zoom to layer extent
#'   \item enter - set zoom to 1
#'   \item ctrl - increase panning speed by 10
#' }
#'
#' @author
#' Stephan Woellauer
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
#' # SpatialPixelsDataFrame
#' plainView(meuse.grid, zcol = "dist")
#'
#'
#' # raster layer
#' m1 <- plainView(poppendorf[[5]])
#' m1
#'
#' # raster stack - true color
#' plainview(poppendorf, 4, 3, 2)
#'
#' @export plainView
#' @name plainView
#' @rdname plainView
#' @aliases plainView,RasterLayer-method

## RasterLayer ============================================================

setMethod('plainView', signature(x = 'RasterLayer'),
          function(x,
                   maxpixels = mapviewGetOption("plainview.maxpixels"),
                   col.regions = mapviewGetOption("raster.palette")(256),
                   at,
                   na.color = mapviewGetOption("na.color"),
                   legend = TRUE,
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

            leg_fl <- NULL

            if (legend) {
              rng <- range(x[], na.rm = TRUE)
              if (missing(at)) at <- lattice::do.breaks(rng, 256)
              leg_fl <- paste0(dir, "/legend", ".png")
              png(leg_fl, height = 200, width = 80, units = "px",
                  bg = "transparent", pointsize = 14, antialias = "none")
              rasterLegend(col = col.regions,
                           at = at,
                           height = 0.9,
                           space = "right")
              dev.off()
            }

            plainViewInternal(filename = fl,
                              imgnm = layer.name,
                              leg_fl = leg_fl,
                              crs = raster::projection(x),
                              dims = c(raster::nrow(x),
                                       raster::ncol(x),
                                       raster::ncell(x)))

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

plainViewInternal <- function(filename, imgnm, crs, dims, leg_fl = NULL) {

  x <- list(imgnm = imgnm,
            crs = crs,
            dims = dims,
            legend = !is.null(leg_fl))

  image_dir <- dirname(filename)
  image_file <- basename(filename)

  attachments <- list(image_file)

  if(!is.null(leg_fl)) {
    legend_dir <- dirname(leg_fl)  #same as image_dir  not checked
    legend_file <- basename(leg_fl)
    attachments <- c(attachments, legend_file)
  }

  dep1 <- htmltools::htmlDependency(name = "image",
                                    version = "1",
                                    src = c(file = image_dir),
                                    attachment = attachments)

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
                   maxpixels = mapviewGetOption("plainview.maxpixels"),
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
                              imgnm = layer.name,
                              crs = raster::projection(x),
                              dims = c(raster::nrow(x),
                                       raster::ncol(x),
                                       raster::ncell(x)))

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


