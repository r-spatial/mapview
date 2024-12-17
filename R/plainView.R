# if ( !isGeneric('plainView') ) {
#   setGeneric('plainView', function(x, ...)
#     standardGeneric('plainView'))
# }
#
# #' View raster objects interactively without background map but in any CRS
# #'
# #' @description
# #' This function is deprecated.
# #' Pleasue use plainview::\code{\link[plainview]{plainView}} instead.
# #'
# #' @param x a \code{\link{raster}}* object
# #' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
# #' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
# #' @param col.regions color (palette).See \code{\link[lattice]{levelplot}} for details.
# #' @param at the breakpoints used for the visualisation. See
# #' \code{\link[lattice]{levelplot}} for details.
# #' @param na.color color for missing values.
# #' @param legend either logical or a list specifying any of the components
# #' decribed in the \code{colorkey} section of \link[lattice]{levelplot}.
# #' @param verbose should some details be printed during the process
# #' @param layer.name the name of the layer to be shown on the map
# #' @param gdal logical. If TRUE (default) gdal_translate is used
# #' to create the png file for display when possible. See details for further
# #' information.
# #' @param ... additional arguments passed on to repective functions.
# #' See \code{\link{addRasterImage}}, \code{\link{addCircles}},
# #' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
# #'
# #' @export plainView
# #' @name plainView
# #' @rdname plainView
# #' @aliases plainView,RasterLayer-method
# #' @importFrom grDevices grey.colors
#
# ## RasterLayer ============================================================
#
# setMethod('plainView', signature(x = 'RasterLayer'),
#           function(x,
#                    maxpixels = mapviewGetOption("plainview.maxpixels"),
#                    col.regions = mapviewGetOption("raster.palette")(256),
#                    at,
#                    na.color = mapviewGetOption("na.color"),
#                    legend = TRUE,
#                    verbose = mapviewGetOption("verbose"),
#                    layer.name = deparse(substitute(x,
#                                                    env = parent.frame())),
#                    gdal = TRUE,
#                    ...) {
#
#             .Defunct(new = "plainview::plainView", package = "mapview")
#
#           }
#
# )
#
#
# ## Raster Stack/Brick ===========================================================
# #' @describeIn plainView \code{\link{stack}} / \code{\link{brick}}
# #'
# #' @param r integer. Index of the Red channel, between 1 and nlayers(x)
# #' @param g integer. Index of the Green channel, between 1 and nlayers(x)
# #' @param b integer. Index of the Blue channel, between 1 and nlayers(x)
#
# setMethod('plainView', signature(x = 'RasterStackBrick'),
#           function(x, r = 3, g = 2, b = 1,
#                    na.color = mapviewGetOption("na.color"),
#                    maxpixels = mapviewGetOption("plainview.maxpixels"),
#                    layer.name = deparse(substitute(x,
#                                                    env = parent.frame())),
#                    ...) {
#
#             .Defunct(new = "plainview::plainView", package = "mapview")
#
#           }
#
# )
#
#
#
# ## SpatialPixelsDataFrame =================================================
# #' @describeIn plainView \code{\link{SpatialPixelsDataFrame}}
# #'
# #' @param zcol attribute name or column number in attribute table
# #' of the column to be rendered
# #'
# setMethod('plainView', signature(x = 'SpatialPixelsDataFrame'),
#           function(x,
#                    zcol = 1,
#                    ...) {
#
#             .Defunct(new = "plainview::plainView", package = "mapview")
#
#           }
# )
#
#
# ## plainview ==============================================================
#
# if ( !isGeneric('plainview') ) {
#   setGeneric('plainview', function(...)
#     standardGeneric('plainview'))
# }
#
# #' @describeIn plainView alias for ease of typing
# #' @aliases plainview
# #' @export plainview
#
# setMethod('plainview', signature('ANY'),
#           function(...) {
#
#             .Defunct(new = "plainview::plainView", package = "mapview")
#
#           }
# )
#
#
