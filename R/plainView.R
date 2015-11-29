if ( !isGeneric('plainView') ) {
  setGeneric('plainView', function(x, ...)
    standardGeneric('plainView'))
}

#' View spatial objects interactively without background map but in any CRS
#'
#' @description
#' this function produces an interactive GIS-like view of the specified
#' spatial object(s) on a plain grey background but for any CRS.
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
#' m1 <- plainView(meuse_rst)
#' m1
#'
#' # factorial RasterLayer
#' m2 <- plainView(raster::as.factor(meuse_rst[[4]]))
#' m2
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
                   colors = mapviewPalette(256),
                   na.color = mapviewGetOption("nacolor"),
                   verbose = mapviewGetOption("verbose"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   ...) {

            x <- rasterCheckSize(x, maxpixels)

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
              png <- raster2PNG(x, colors = color,
                                na.color = na.color,
                                maxpixels = maxpixels)
              cat("write png\n")
              png::writePNG(png, fl)
            #}

            layer.name <- deparse(substitute(img, env = parent.frame()))
cat("plainViewInternal\n")
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
