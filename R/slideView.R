if ( !isGeneric('slideView') ) {
  setGeneric('slideView', function(x, y, ...)
    standardGeneric('slideView'))
}

#' Compare two images trhough interactive swiping overlay
#'
#' @description
#' Two images are overlaid and a slider is provided to interactively
#' compare the two images in a before-after like fashion. \code{x} and
#' \code{y} can either be two RasterLayers, two RasterBricks/Stacks or
#' two character strings. In the latter case it is assumed that these
#' point to .png images on the disk.
#'
#' This is a modified implementation of http://bl.ocks.org/rfriberg/8327361
#'
#' @param x a RasterStack/Brick, RasterLayer or path to a .png file
#' @param y a RasterStack/Brick, RasterLayer or path to a .png file
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param colors the color palette to be used for visualising RasterLayers
#' @param na.color the color to be used for NA pixels
#'
#' @author
#' Tim Appelhans
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
#' slideView(stck1, stck2)
#'
#' @export
#' @docType methods
#' @name slideView
#' @rdname slideView
#' @aliases slideView,RasterStackBrick,RasterStackBrick-method
NULL
setMethod("slideView", signature(x = "RasterStackBrick",
                                 y = "RasterStackBrick"),
          function(x, y,
                   maxpixels = 500000) {

            png1 <- rgbStack2PNG(x, maxpixels = maxpixels)
            png2 <- rgbStack2PNG(y, maxpixels = maxpixels)

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
            viewer <- getOption("viewer")
            if (!is.null(viewer))
              viewer(htmlFile)
            else
              utils::browseURL(htmlFile)

          }

)

## RasterLayers ===========================================================
#' @describeIn slideView

setMethod("slideView", signature(x = "RasterLayer",
                                 y = "RasterLayer"),
          function(x,
                   y,
                   colors = mapViewPalette(7),
                   na.color = "#00000000",
                   maxpixels = 500000) {

            png1 <- raster2PNG(x, colors = colors,
                               na.color = na.color,
                               maxpixels = maxpixels)
            png2 <- raster2PNG(y, colors = colors,
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
            viewer <- getOption("viewer")
            if (!is.null(viewer))
              viewer(htmlFile)
            else
              utils::browseURL(htmlFile)

          }

)


## png files ==============================================================
#' @describeIn slideView

setMethod("slideView", signature(x = "character",
                                 y = "character"),
          function(x, y) {

            png1 <- png::readPNG(x)
            png2 <- png::readPNG(y)

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
            viewer <- getOption("viewer")
            if (!is.null(viewer))
              viewer(htmlFile)
            else
              utils::browseURL(htmlFile)

          }

)
