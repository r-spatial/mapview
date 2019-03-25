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
#' @param legend either logical or a list specifying any of the components
#' decribed in the \code{colorkey} section of \link[lattice]{levelplot}.
#' @param verbose should some details be printed during the process
#' @param layer.name the name of the layer to be shown on the map
#' @param gdal logical. If TRUE (default) gdal_translate is used
#' to create the png file for display when possible. See details for further
#' information.
#' @param ... additional arguments passed on to repective functions.
#' See \code{\link{addRasterImage}}, \code{\link{addCircles}},
#' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
#'
#' @details
#' If the raster object is not in memory
#' (i.e. if \code{raster::inMemory} is \code{FLASE})
#' and argument \code{gdal} is set to TRUE (default) gdal_translate
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
#' if (interactive()) {
#'  ### raster data ###
#'  library(sp)
#'  library(raster)
#'
#'  data(meuse.grid)
#'  coordinates(meuse.grid) = ~x+y
#'  proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#'  gridded(meuse.grid) = TRUE
#'  meuse_rst <- stack(meuse.grid)
#'
#'  # SpatialPixelsDataFrame
#'  plainView(meuse.grid, zcol = "dist")
#'
#'
#'  # raster layer
#'  m1 <- plainView(poppendorf[[5]])
#'  m1
#'
#'  # raster stack - true color
#'  plainview(poppendorf, 4, 3, 2)
#' }
#'
#' @export plainView
#' @name plainView
#' @rdname plainView
#' @aliases plainView,RasterLayer-method
#' @importFrom grDevices grey.colors

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

            .Deprecated(new = "plainview::plainView", package = "mapview",
                        old = "mapview::plainView")

            plainview::plainView(
              x = x,
              maxpixels = maxpixels,
              col.regions = col.regions,
              at = at,
              na.color = na.color,
              legend = legend,
              verbose = verbose,
              layer.name = layer.name,
              gdal = gdal,
              ...
            )

            # ## temp dir
            # dir <- tempfile()
            # dir.create(dir)
            # fl <- paste0(dir, "/img", ".png")
            #
            # if (raster::fromDisk(x) & gdal) {
            #   # gdalUtils::gdal_translate(src_dataset = raster::filename(x),
            #   #                           dst_dataset = fl,
            #   #                           of = "PNG",
            #   #                           b = raster::bandnr(x),
            #   #                           verbose = verbose)
            #   tmp = sf::gdal_utils(util = "translate",
            #                        source = raster::filename(x),
            #                        destination = fl,
            #                        options = c("-of", "PNG", "-b",
            #                                    as.character(raster::bandnr(x)),
            #                                    "-scale", "-ot", "Byte"))
            # } else {
            #   png <- raster2PNG(x,
            #                     col.regions = col.regions,
            #                     at = at,
            #                     na.color = na.color,
            #                     maxpixels = maxpixels)
            #
            #   png::writePNG(png, fl)
            # }
            #
            # leg_fl <- NULL
            #
            # if (!is_literally_false(legend)) {
            #   if (raster::fromDisk(x) & gdal) {
            #     col.regions = grDevices::grey.colors(256, start = 0, end = 1, gamma = 1)
            #   } else {
            #     col.regions = col.regions
            #   }
            #   rng <- range(x[], na.rm = TRUE)
            #   if (missing(at)) at <- lattice::do.breaks(rng, 256)
            #
            #   if (isTRUE(legend)) {
            #     legend = list(NULL)
            #   }
            #     key = list(col = col.regions,
            #                at = at,
            #                height = 0.9,
            #                space = "right")
            #   # }
            #
            #   key = utils::modifyList(key, legend)
            #
            #   leg_fl <- paste0(dir, "/legend", ".png")
            #   png(leg_fl, height = 200, width = 80, units = "px",
            #       bg = "transparent", pointsize = 14, antialias = "none")
            #   rasterLegend(key)
            #   dev.off()
            # }
            #
            # plainViewInternal(filename = fl,
            #                   imgnm = layer.name,
            #                   leg_fl = leg_fl,
            #                   crs = raster::projection(x),
            #                   dims = c(raster::nrow(x),
            #                            raster::ncol(x),
            #                            raster::ncell(x)))

          }

)


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

            .Deprecated(new = "plainview::plainView", package = "mapview",
                        old = "mapview::plainView")

            plainview::plainView(
              x = x,
              r = r,
              g = g,
              b = b,
              na.color = na.color,
              maxpixels = maxpixels,
              layer.name = layer.name,
              ...
            )

            # ## temp dir
            # dir <- tempfile()
            # dir.create(dir)
            # fl <- paste0(dir, "/img", ".png")
            #
            # #             if (raster::filename(x) != "") {
            # #               gdalUtils::gdal_translate(src_dataset = filename(x),
            # #                                         dst_dataset = fl,
            # #                                         of = "PNG",
            # #                                         verbose = TRUE)
            # #             } else {
            # png <- rgbStack2PNG(x, r = r, g = g, b = b,
            #                     na.color = na.color,
            #                     maxpixels = maxpixels,
            #                     ...)
            # png::writePNG(png, fl)
            # #}
            #
            # layer.name <- paste0(layer.name, "_", r, ".", g, ".", b)
            # plainViewInternal(filename = fl,
            #                   imgnm = layer.name,
            #                   crs = raster::projection(x),
            #                   dims = c(raster::nrow(x),
            #                            raster::ncol(x),
            #                            raster::ncell(x)))

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

            .Deprecated(new = "plainview::plainView", package = "mapview",
                        old = "mapview::plainView")

            plainview::plainView(
              x = x,
              zcol = zcol,
              ...
            )

            # if (is.character(zcol)) nm <- zcol else  nm <- names(x)[zcol]
            # x <- raster::raster(x[zcol])
            #
            # plainView(x, layer.name = nm, ...)

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
          function(...) {

            .Deprecated(new = "plainview::plainView", package = "mapview",
                        old = "mapview::plainView")

            plainview::plainView(...)

          }
)

