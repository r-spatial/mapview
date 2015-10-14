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

            png1 <- rgbStack2PNG(img1, maxpixels = maxpixels)
            png2 <- rgbStack2PNG(img2, maxpixels = maxpixels)

            ## temp dir
            dir <- tempfile()
            dir.create(dir)
            fl1 <- paste0(dir, "/img1", ".png")
            fl2 <- paste0(dir, "/img2", ".png")

            ## d3
            d3_pth <- system.file("d3", package = "mapview")
            d3_fls <- list.files(d3_pth, pattern = glob2rx("*.js"),
                                 full.names = TRUE)
            tmp <- file.copy(d3_fls, dir)

            ## template
            tmplt_pth <- system.file("d3/templates", package = "mapview")
            tmplt_fl <- list.files(tmplt_pth, pattern = "slider.html",
                                   full.names = TRUE)
            tmplt <- paste(readLines(tmplt_fl), collapse = "\n")
            htmlFile <- file.path(dir, "index.html")
            write(tmplt, htmlFile)

            ## pngs
            png::writePNG(png1, fl1)
            png::writePNG(png2, fl2)

            ## view
            #viewer <- getOption("viewer")
            #if (!is.null(viewer))
            #  viewer(htmlFile)
            #else
            #  utils::browseURL(htmlFile)

            slideView2internal(list(a="a", b="b"), filename1 = fl1, filename2 = fl2)


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
            png1 <- raster2PNG(img1, colors = colors,
                               na.color = na.color,
                               maxpixels = maxpixels)
            png2 <- raster2PNG(img2, colors = colors,
                               na.color = na.color,
                               maxpixels = maxpixels)

            ## temp dir
            dir <- tempfile()
            dir.create(dir)
            fl1 <- paste0(dir, "/img1", ".png")
            fl2 <- paste0(dir, "/img2", ".png")

            ## d3
            d3_pth <- system.file("d3", package = "mapview")
            d3_fls <- list.files(d3_pth, pattern = glob2rx("*.js"),
                                 full.names = TRUE)
            tmp <- file.copy(d3_fls, dir)

            ## template
            tmplt_pth <- system.file("d3/templates", package = "mapview")
            tmplt_fl <- list.files(tmplt_pth, pattern = "slider.html",
                                   full.names = TRUE)
            tmplt <- paste(readLines(tmplt_fl), collapse = "\n")
            htmlFile <- file.path(dir, "index.html")
            write(tmplt, htmlFile)

            ## pngs
            png::writePNG(png1, fl1)
            png::writePNG(png2, fl2)

            ## view
            #viewer <- getOption("viewer")
            #if (!is.null(viewer))
            #  viewer(htmlFile)
            #else
            #  utils::browseURL(htmlFile)

            slideView2internal(list(a="a", b="b"), filename1 = fl1, filename2 = fl2)

          }

)


## png files ==============================================================
#' @describeIn slideView2

setMethod("slideView2", signature(img1 = "character",
                                  img2 = "character"),
          function(img1, img2) {

            png1 <- png::readPNG(img1)
            png2 <- png::readPNG(img2)

            ## temp dir
            dir <- tempfile()
            dir.create(dir)
            fl1 <- paste0(dir, "/img1", ".png")
            fl2 <- paste0(dir, "/img2", ".png")

            ## d3
            d3_pth <- system.file("d3", package = "mapview")
            d3_fls <- list.files(d3_pth, pattern = glob2rx("*.js"),
                                 full.names = TRUE)
            tmp <- file.copy(d3_fls, dir)

            ## template
            tmplt_pth <- system.file("d3/templates", package = "mapview")
            tmplt_fl <- list.files(tmplt_pth, pattern = "slider.html",
                                   full.names = TRUE)
            tmplt <- paste(readLines(tmplt_fl), collapse = "\n")
            htmlFile <- file.path(dir, "index.html")
            write(tmplt, htmlFile)

            ## pngs
            png::writePNG(png1, fl1)
            png::writePNG(png2, fl2)

            ## view
            #viewer <- getOption("viewer")
            #if (!is.null(viewer))
            #  viewer(htmlFile)
            #else
            #  utils::browseURL(htmlFile)

            slideView2internal(list(a="a", b="b"), filename1 = fl1, filename2 = fl2)

          }

)

#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
slideView2internal <- function(message, width = NULL, height = NULL, filename1 = NULL, filename2 = NULL) {

  # forward options using x
  x = list(
    message = message
  )

  #filename1 and filename2 need to have same directory!
  test_dir = dirname(filename1)

  test_file1 = basename(filename1)
  test_file2 = basename(filename2)

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
