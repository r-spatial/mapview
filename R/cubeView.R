#' View a RasterStack or RasterBrick as 3-dimensional data cube.
#'
#' @description
#' Create a 3D data cube from a RasterStack or RasterBrick. The cube can be
#' freely rotated so that Hovmoller views of x - z and y - z are possible.
#'
#' @param x a RasterStack or RasterBrick
#' @param at the breakpoints used for the visualisation. See
#' \code{\link{levelplot}} for details.
#' @param col.regions color (palette).See \code{\link{levelplot}} for details.
#'
#' @details
#' The visible layers are alterable by keys: \cr
#' x-axis: LEFT / RIGHT arrow key \cr
#' y-axis: DOWN / UP arrow key \cr
#' z-axis: PAGE_DOWN / PAGE_UP key \cr
#'
#' Note: In RStudio cubeView may show a blank viewer window. In this case open the view in
#' a web-browser (RStudio button at viewer: "show in new window").#'
#'
#' Note: Because of key focus issues key-press-events are not always
#' recognised within RStudio at Windows. In this case open the view in
#' a web-browser (RStudio button at viewer: "show in new window").
#'
#' Press and hold left mouse-button to rotate the cube.
#' Press and hold right mouse-button to move the cube.
#' Spin mouse-wheel or press and hold middle mouse-button and
#' move mouse down/up to zoom the cube.
#'
#' @author
#' Stephan Woellauer and Tim Appelhans
#'
#' @examples
#' \dontrun{
#' library(raster)
#'
#' kili_data <- system.file("extdata", "kiliNDVI.tif", package = "mapview")
#' kiliNDVI <- stack(kili_data)
#'
#' cubeView(kiliNDVI)
#'
#' library(RColorBrewer)
#' clr <- colorRampPalette(brewer.pal(9, "BrBG"))
#' cubeView(kiliNDVI, at = seq(-0.15, 0.95, 0.1), col.regions = clr)
#' }
#'
#' @export cubeView
#' @name cubeView

cubeView <- function(x,
                     at,
                     col.regions = mapviewGetOption("raster.palette"),
                     legend = TRUE) {

  stopifnot(inherits(x, "RasterStack") | inherits(x, "RasterBrick"))

  #v <- raster::as.matrix(flip(x, direction = "y"))
  v <- raster::as.matrix(x)
  if (missing(at)) at <- lattice::do.breaks(range(v, na.rm = TRUE), 256)
  cols <- lattice::level.colors(v,
                                at = at,
                                col.regions)
  tst <- grDevices::col2rgb(cols, alpha = TRUE)

  x_size <- raster::ncol(x)
  y_size <- raster::nrow(x)
  z_size <- raster::nlayers(x)

  leg_fl <- NULL

  if (legend) {
    ## unique temp dir
    dir <- tempfile()
    dir.create(dir)
    rng <- range(x[], na.rm = TRUE)
    if (missing(at)) at <- lattice::do.breaks(rng, 256)
    leg_fl <- paste0(dir, "/legend", ".png")
    png(leg_fl, height = 200, width = 80, units = "px",
        bg = "transparent", pointsize = 14)
    rasterLegend(col = col.regions,
                 at = at,
                 height = 0.9,
                 space = "right")
    dev.off()
  }


  cubeViewRaw(red = tst[1, ],
              green = tst[2, ],
              blue = tst[3, ],
              x_size = x_size,
              y_size = y_size,
              z_size = z_size,
              leg_fl = leg_fl)

}

# ' View a cube of 3-dimensional data filled with points (voxels).
# '
# ' A variation of Hovmoeller diagram: Each voxel is colored with a RGB-color (or grey) value.
# '
# ' @param x_size integer. size of x-dimension
# '
# ' @param y_size integer. size of y-dimension
# '
# ' @param z_size integer. size of z-dimension
# '
# ' @param grey optional integer vector with 0 <= value <= 255.
# '
# ' @param red optional integer vector with 0 <= value <= 255.
# '
# ' @param green optional integer vector with 0 <= value <= 255.
# '
# ' @param blue optional integer vector with 0 <= value <= 255.
# '
# ' @details
# '
# ' The cube faces show a selectable layer of data within the cube.
# '
# ' The visible layers are alterable by keys:
# '
# ' x-axis: LEFT / RIGHT arrow key
# '
# ' y-axis: DOWN / UP arrow key
# '
# ' z-axis: PAGE_DOWN / PAGE_UP key
# '
# ' Note: Because of key focus issues key-press-events are not always
# ' recognised within RStudio at Windows.
# ' In this case open the view in a web-browser (RStudio button: "show in new window").
# '
# '
# ' Press and hold left mouse-button to rotate the cube.
# '
# ' Press and hold right mouse-button to move the cube.
# '
# ' Spin mouse-wheel or press and hold middle mouse-button and move mouse
# ' down/up to zoom the cube.
# '
# ' Press SPACE to toggle showing cross section lines on the cube.
# '
# ' The color resp. grey vectors contain sequentially values of each voxel.
# ' So each vector is length == x_size * y_size * z_size.
# ' Color component values overwrite grey values.
# '
# ' Sequence of coordinates (x,y,z) for values in vectors:
# '
# ' (1,1,1), (2,1,1), (3,1,1), ... (1,2,1), (2,2,1), (3,2,1), ... (1,1,2), (2,1,2), (3,1,2), ...
# '
# '
# ' @author
# ' Stephan Woellauer
# '
# ' @import htmlwidgets
# '
# ' @export
cubeViewRaw <- function(grey = NULL,
                        red = NULL,
                        green = NULL,
                        blue = NULL,
                        x_size,
                        y_size,
                        z_size,
                        width = NULL,
                        height = NULL,
                        leg_fl = NULL) {

  total_size <- x_size*y_size*z_size

  object_list <- list(x_size = x_size,
                      y_size = y_size,
                      z_size = z_size,
                      legend = !is.null(leg_fl))

  if(!is.null(grey)) {
    if(length(grey)!=total_size) {
      stop("length of grey vector not correct: ", length(grey), " should be ", total_size)
    }
    object_list <- c(object_list, list(grey=as.raw(as.integer(grey))))
  }

  if(!is.null(red)) {
    if(length(red)!=total_size) {
      stop("length of red vector not correct: ", length(red), " should be ", total_size)
    }
    object_list <- c(object_list, list(red=as.raw(as.integer(red))))
  }

  if(!is.null(green)) {
    if(length(green)!=total_size) {
      stop("length of green vector not correct: ", length(green), " should be ", total_size)
    }
    object_list <- c(object_list, list(green=as.raw(as.integer(green))))
  }

  if(!is.null(blue)) {
    if(length(blue)!=total_size) {
      stop("length of blue vector not correct: ", length(blue), " should be ", total_size)
    }
    object_list <- c(object_list, list(blue=as.raw(as.integer(blue))))
  }

  deps <- list()

  if(!is.null(leg_fl)) {
    images_dir <- dirname(leg_fl)
    legend_file <- basename(leg_fl)
    attachments <- list(legend=legend_file)
    dep1 <- htmltools::htmlDependency(name = "images", version = "1", src = c(file = images_dir), attachment = attachments, all_files = FALSE)
    deps <- list(dep1)
  }

  # create widget
  htmlwidgets::createWidget(
    name = 'cubeView',
    x = object_list,
    width = width,
    height = height,
    package = 'mapview',
    sizingPolicy = htmlwidgets::sizingPolicy(padding = 0, browser.fill = TRUE),
    dependencies = deps
  )
}

#' Widget output function for use in Shiny
#'
#' @param outputId Output variable to read from
#' @param width,height the width and height of the map
#' (see \code{\link{shinyWidgetOutput}})
#'
#' @export
cubeViewOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'cubeView',
                                 width, height, package = 'mapview')
}

#' Widget render function for use in Shiny
#'
#' @param expr An expression that generates an HTML widget
#' @param env The environment in which to evaluate expr
#' @param quoted Is expr a quoted expression (with quote())?
#' This is useful if you want to save an expression in a variable
#'
#' @export
renderCubeView <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, cubeViewOutput, env, quoted = TRUE)
}


## cubeview ===============================================================
#' @describeIn cubeView alias for ease of typing
#' @aliases cubeview
#' @export cubeview

cubeview <-  function(...) cubeView(...)

