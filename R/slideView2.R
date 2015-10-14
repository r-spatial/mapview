if ( !isGeneric('slideView2') ) {
  setGeneric('slideView2', function(img1, img2, ...)
    standardGeneric('slideView2'))
}

#' TODO Compare two images trough interactive swiping overlay
#'
#' @description
#' Two images are overlaid and a slider is provided to interactively
#' compare the two images in a before-after like fashion. \code{img1} and
#' \code{img2} can either be two RasterLayers, two RasterBricks/Stacks or
#' two character strings. In the latter case it is assumed that these
#' point to .png images on the disk.
#'
#' This is a modified implementation of http://bl.ocks.org/rfriberg/8327361
#'
#' @param img1 a RasterStack/Brick, RasterLayer or path to a .png file
#' @param img2 a RasterStack/Brick, RasterLayer or path to a .png file
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param colors the color palette to be used for visualising RasterLayers
#' @param na.color the color to be used for NA pixels
#'
#' @author
#' Tim Appelhans
#' Stephan Woellauer
#'
#' @examples
#' ### raster data ###
#' library(sp)
#' library(raster)
#'
#' data(poppendorf)
#'
#' stck1 <- subset(poppendorf, c(3, 4, 5))
#' stck2 <- subset(poppendorf, c(2, 3, 4))
#' slideView2(stck1, stck2)
#'
#' @export
#' @docType methods
#' @name slideView2
#' @rdname slideView2
#' @aliases slideView2,RasterStackBrick,RasterStackBrick-method
NULL
setMethod("slideView2", signature(img1 = "RasterStackBrick",
                                  img2 = "RasterStackBrick"),
          function(img1, img2,
                   maxpixels = 500000) {

          }

)

## RasterLayers ===========================================================
#' @describeIn slideView2

setMethod("slideView2", signature(img1 = "RasterLayer",
                                  img2 = "RasterLayer"),
          function(img1,
                   img2,
                   colors = mapViewPalette(7),
                   na.color = "#00000000",
                   maxpixels = 500000) {


          }

)


## png files ==============================================================
#' @describeIn slideView2

setMethod("slideView2", signature(img1 = "character",
                                  img2 = "character"),
          function(img1, img2) {


          }

)




#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
slideView2internal <- function(message, width = NULL, height = NULL) {

  # forward options using x
  x = list(
    message = message
  )

  test_dir = "c:/temp2"
  test_file1 = "test1.png"
  test_file2 = "test2.png"

  dep1 <- htmltools::htmlDependency(name="test", version="1", src = c(file=test_dir), attachment = list(test_file1, test_file2) )
  deps <- list(dep1)

  # create widget
  htmlwidgets::createWidget(
    name = 'slideView2',
    x,
    width = width,
    height = height,
    package = 'mapview',
    dependencies = deps
  )
}

#' Widget output function for use in Shiny
#'
#' @export
slideView2Output <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'slideView2', width, height, package = 'mapview')
}

#' Widget render function for use in Shiny
#'
#' @export
renderSlideView2 <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, slideView2Output, env, quoted = TRUE)
}
