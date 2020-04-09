if ( !isGeneric('slideView') ) {
  setGeneric('slideView', function(img1, img2, ...)
    standardGeneric('slideView'))
}

#' Compare two images trough interactive swiping overlay
#'
#' @title slideView
#'
#' @description
#' This function is deprecated.
#' Please use slideview::\code{\link[slideview]{slideView}} instead.
#'
#' @param img1 a RasterStack/Brick, RasterLayer or path to a .png file
#' @param img2 a RasterStack/Brick, RasterLayer or path to a .png file
#' @param label1 slider label for img1 (defaults to object name)
#' @param label2 slider label for img2 (defaults to object name)
#' @param r integer. Index of the Red channel, between 1 and nlayers(x)
#' @param g integer. Index of the Green channel, between 1 and nlayers(x)
#' @param b integer. Index of the Blue channel, between 1 and nlayers(x)
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param color the color palette to be used for visualising RasterLayers
#' @param na.color the color to be used for NA pixels
#' @param col.regions color (palette).See \code{\link{levelplot}} for details.
#' @param legend whether to plot legends for the two images (ignored for
#' RatserStacks/*Bricks).
#' @param ... additional arguments passed on to repective functions.
#'
#' @export
#' @docType methods
#' @name slideView
#' @rdname slideView
#' @aliases slideView,RasterStackBrick,RasterStackBrick-method

setMethod("slideView", signature(img1 = "RasterStackBrick",
                                 img2 = "RasterStackBrick"),
          function(img1,
                   img2,
                   label1 = deparse(substitute(img1, env = parent.frame())),
                   label2 = deparse(substitute(img2, env = parent.frame())),
                   r = 3,
                   g = 2,
                   b = 1,
                   na.color = mapviewGetOption("na.color"),
                   maxpixels = mapviewGetOption("plainview.maxpixels"),
                   ...) {

            .Defunct(new = "slideview::slideView", package = "mapview")

          }

)

## RasterLayers ===========================================================
#' @describeIn slideView for RasterLayers
#'
setMethod("slideView", signature(img1 = "RasterLayer",
                                 img2 = "RasterLayer"),
          function(img1,
                   img2,
                   label1 = deparse(substitute(img1, env = parent.frame())),
                   label2 = deparse(substitute(img2, env = parent.frame())),
                   legend = TRUE,
                   col.regions = mapviewGetOption("raster.palette")(256),
                   na.color = mapviewGetOption("na.color"),
                   maxpixels = mapviewGetOption("plainview.maxpixels")) {

            .Defunct(new = "slideview::slideView", package = "mapview")

          }

)

## RasterStackBrick, RasterLayer ===========================================================
#' @describeIn slideView for RasterStackBrick, RasterLayer
#'
setMethod("slideView", signature(img1 = "RasterStackBrick",
                                 img2 = "RasterLayer"),
          function(img1,
                   img2,
                   label1 = deparse(substitute(img1, env = parent.frame())),
                   label2 = deparse(substitute(img2, env = parent.frame())),
                   legend = TRUE,
                   r = 3,
                   g = 2,
                   b = 1,
                   col.regions = mapviewGetOption("raster.palette")(256),
                   na.color = mapviewGetOption("na.color"),
                   maxpixels = mapviewGetOption("plainview.maxpixels"),
                   ...) {

            .Defunct(new = "slideview::slideView", package = "mapview")

          }

)



## RasterLayer, RasterStackBrick ===========================================================
#' @describeIn slideView for RasterLayer, RasterStackBrick
#'
setMethod("slideView", signature(img1 = "RasterLayer",
                                 img2 = "RasterStackBrick"),
          function(img1,
                   img2,
                   label1 = deparse(substitute(img1, env = parent.frame())),
                   label2 = deparse(substitute(img2, env = parent.frame())),
                   legend = TRUE,
                   r = 3,
                   g = 2,
                   b = 1,
                   col.regions = mapviewGetOption("raster.palette")(256),
                   na.color = mapviewGetOption("na.color"),
                   maxpixels = mapviewGetOption("plainview.maxpixels"),
                   ...) {

            .Defunct(new = "slideview::slideView", package = "mapview")

          }

)


## png files ==============================================================
#' @describeIn slideView for png files

setMethod("slideView", signature(img1 = "character",
                                 img2 = "character"),
          function(img1, img2,
                   label1 = deparse(substitute(img1, env = parent.frame())),
                   label2 = deparse(substitute(img2, env = parent.frame()))) {

            .Defunct(new = "slideview::slideView", package = "mapview")

          }

)

#' Widget output function for use in Shiny
#'
#' @param outputId Output variable to read from
#' @param width,height the width and height of the canas element
#' (see \code{\link{shinyWidgetOutput}})
#'
#' @export
slideViewOutput <- function(outputId, width = '100%', height = '400px'){
  .Defunct(new = "slideview::slideViewOutput", package = "mapview")
}

#' Widget render function for use in Shiny
#'
#' @param expr An expression that generates an HTML widget
#' @param env The environment in which to evaluate expr
#' @param quoted Is expr a quoted expression (with quote())?
#' This is useful if you want to save an expression in a variable
#'
#' @export
renderslideView <- function(expr, env = parent.frame(), quoted = FALSE) {
  .Defunct(new = "slideview::renderslideView", package = "mapview")
}


## slideview ==============================================================

if ( !isGeneric('slideview') ) {
  setGeneric('slideview', function(...)
    standardGeneric('slideview'))
}

#' @describeIn slideView alias for ease of typing
#' @aliases slideview
#' @export slideview

setMethod('slideview', signature('ANY'),
          function(...) {
            .Defunct(new = "slideview::slideview", package = "mapview")
          })

