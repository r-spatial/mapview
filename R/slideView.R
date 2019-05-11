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

            .Deprecated(new = "slideview::slideView", package = "mapview",
                        old = "mapview::slideView")

            png1 <- rgbStack2PNG(img1, r = r, g = g, b = b,
                                 na.color = na.color,
                                 maxpixels = maxpixels,
                                 ...)
            png2 <- rgbStack2PNG(img2, r = r, g = g, b = b,
                                 na.color = na.color,
                                 maxpixels = maxpixels,
                                 ...)

            ## temp dir
            dir <- tempfile()
            dir.create(dir)
            # r1 <- trunc(runif(1, 1000000000, 9999999999))
            # r2 <- trunc(runif(1, 1000000000, 9999999999))
            fl1 <- paste0(dir, "/", label1, ".png")
            fl2 <- paste0(dir, "/", label2, ".png")

            ## pngs
            png::writePNG(png1, fl1)
            png::writePNG(png2, fl2)

            slideViewInternal(list(a="a", b="b"),
                              img1nm = label1,
                              img2nm = label2,
                              filename1 = fl1,
                              filename2 = fl2)
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

            .Deprecated(new = "slideview::slideView", package = "mapview",
                        old = "mapview::slideView")

            png1 <- raster2PNG(img1, col.regions = col.regions,
                               na.color = na.color,
                               maxpixels = maxpixels)
            png2 <- raster2PNG(img2, col.regions = col.regions,
                               na.color = na.color,
                               maxpixels = maxpixels)

            ## temp dir
            dir <- tempfile()
            dir.create(dir)
            # r1 <- trunc(runif(1, 1000000000, 9999999999))
            # r2 <- trunc(runif(1, 1000000000, 9999999999))
            fl1 <- paste0(dir, "/", label1, ".png")
            fl2 <- paste0(dir, "/", label2, ".png")

            ## pngs
            png::writePNG(png1, fl1)
            png::writePNG(png2, fl2)

            leg_flr <- NULL
            leg_fll <- NULL

            # legend <- TRUE # !! testing !!
            if (legend) {
              ## legend one (right)
              rngr <- range(img1[], na.rm = TRUE)
              # if (missing(at)) at <- lattice::do.breaks(rng, 256)
              atr <- lattice::do.breaks(rngr, 256)
              leg_flr <- paste0(dir, "/legendr", label1, ".png")
              png(leg_flr, height = 200, width = 80, units = "px",
                  bg = "transparent", pointsize = 14)
              rasterLegend(
                list(
                  col = col.regions,
                  at = atr,
                  height = 0.9,
                  space = "right"
                )
              )
              dev.off()

              ## legend two (left)
              rngl <- range(img2[], na.rm = TRUE)
              # if (missing(at)) at <- lattice::do.breaks(rng, 256)
              atl <- lattice::do.breaks(rngl, 256)
              leg_fll <- paste0(dir, "/legendl", label2, ".png")
              png(leg_fll, height = 200, width = 80, units = "px",
                  bg = "transparent", pointsize = 14)
              rasterLegend(
                list(
                  col = col.regions,
                  at = atl,
                  height = 0.9,
                  space = "left"
                )
              )
              dev.off()
            }

            slideViewInternal(list(a="a", b="b"),
                              img1nm = label1,
                              img2nm = label2,
                              filename1 = fl1,
                              filename2 = fl2,
                              leg_flr = leg_flr,
                              leg_fll = leg_fll)
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

            .Deprecated(new = "slideview::slideView", package = "mapview",
                        old = "mapview::slideView")

            png1 <- rgbStack2PNG(img1, r = r, g = g, b = b,
                                 na.color = na.color,
                                 maxpixels = maxpixels,
                                 ...)
            png2 <- raster2PNG(img2, col.regions = col.regions,
                               na.color = na.color,
                               maxpixels = maxpixels)

            ## temp dir
            dir <- tempfile()
            dir.create(dir)
            # r1 <- trunc(runif(1, 1000000000, 9999999999))
            # r2 <- trunc(runif(1, 1000000000, 9999999999))
            fl1 <- paste0(dir, "/", label1, ".png")
            fl2 <- paste0(dir, "/", label2, ".png")

            ## pngs
            png::writePNG(png1, fl1)
            png::writePNG(png2, fl2)

            leg_flr <- NULL
            leg_fll <- NULL

            if (legend) {
              ## legend two (left)
              rngl <- range(img2[], na.rm = TRUE)
              # if (missing(at)) at <- lattice::do.breaks(rng, 256)
              atl <- lattice::do.breaks(rngl, 256)
              leg_fll <- paste0(dir, "/legendl", label2, ".png")
              png(leg_fll, height = 200, width = 80, units = "px",
                  bg = "transparent", pointsize = 14)
              rasterLegend(
                list(
                  col = col.regions,
                  at = atl,
                  height = 0.9,
                  space = "left"
                )
              )
              dev.off()
            }

            slideViewInternal(list(a="a", b="b"),
                              img1nm = label1,
                              img2nm = label2,
                              filename1 = fl1,
                              filename2 = fl2,
                              leg_flr = leg_flr,
                              leg_fll = leg_fll)
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

            .Deprecated(new = "slideview::slideView", package = "mapview",
                        old = "mapview::slideView")

            png1 <- raster2PNG(img1, col.regions = col.regions,
                               na.color = na.color,
                               maxpixels = maxpixels)

            png2 <- rgbStack2PNG(img2, r = r, g = g, b = b,
                                 na.color = na.color,
                                 maxpixels = maxpixels,
                                 ...)

            ## temp dir
            dir <- tempfile()
            dir.create(dir)
            # r1 <- trunc(runif(1, 1000000000, 9999999999))
            # r2 <- trunc(runif(1, 1000000000, 9999999999))
            fl1 <- paste0(dir, "/", label1, ".png")
            fl2 <- paste0(dir, "/", label2, ".png")

            ## pngs
            png::writePNG(png1, fl1)
            png::writePNG(png2, fl2)

            leg_flr <- NULL
            leg_fll <- NULL

            if (legend) {
              ## legend one (right)
              rngr <- range(img1[], na.rm = TRUE)
              # if (missing(at)) at <- lattice::do.breaks(rng, 256)
              atr <- lattice::do.breaks(rngr, 256)
              leg_flr <- paste0(dir, "/legendr", label1, ".png")
              png(leg_flr, height = 200, width = 80, units = "px",
                  bg = "transparent", pointsize = 14)
              rasterLegend(
                list(
                  col = col.regions,
                  at = atr,
                  height = 0.9,
                  space = "right"
                )
              )
              dev.off()
            }

            slideViewInternal(list(a="a", b="b"),
                              img1nm = label1,
                              img2nm = label2,
                              filename1 = fl1,
                              filename2 = fl2,
                              leg_flr = leg_flr,
                              leg_fll = leg_fll)
          }

)


## png files ==============================================================
#' @describeIn slideView for png files

setMethod("slideView", signature(img1 = "character",
                                 img2 = "character"),
          function(img1, img2,
                   label1 = deparse(substitute(img1, env = parent.frame())),
                   label2 = deparse(substitute(img2, env = parent.frame()))) {

            .Deprecated(new = "slideview::slideView", package = "mapview",
                        old = "mapview::slideView")

            png1 <- png::readPNG(img1)
            png2 <- png::readPNG(img2)

            ## temp dir
            dir <- tempfile()
            dir.create(dir)
            # r1 <- trunc(runif(1, 1000000000, 9999999999))
            # r2 <- trunc(runif(1, 1000000000, 9999999999))
            fl1 <- paste0(dir, "/", label1, ".png")
            fl2 <- paste0(dir, "/", label2, ".png")

            ## pngs
            png::writePNG(png1, fl1)
            png::writePNG(png2, fl2)

            slideViewInternal(list(a="a", b="b"),
                              img1nm = label1,
                              img2nm = label2,
                              filename1 = fl1,
                              filename2 = fl2)
          }

)


### internal functions

slideViewInternal <- function(message,
                              img1nm = NULL,
                              img2nm = NULL,
                              width = NULL,
                              height = NULL,
                              filename1 = NULL,
                              filename2 = NULL,
                              leg_flr = NULL,
                              leg_fll = NULL) {


  nm = paste0(img1nm, "-", img2nm)
  # forward options using x
  x <- list(
    message = message,
    img1 = img1nm,
    img2 = img2nm,
    legend = (!is.null(leg_flr)) || (!is.null(leg_fll)),
    fldrnm = nm
  )

  #filename1 and filename2 need to have same directory!
  image_dir <- dirname(filename1)

  image_file1 <- basename(filename1)
  image_file2 <- basename(filename2)

  attachments = list(imager = image_file1, imagel = image_file2)

  if( !is.null(leg_flr) ) {
    legendr_dir <- dirname(leg_flr)  #same as image_dir  not checked
    legendr_file <- basename(leg_flr)
    attachments <- c(attachments, legendr = legendr_file)
  }

  if( !is.null(leg_fll) ) {
    legendl_dir <- dirname(leg_fll)  #same as image_dir  not checked
    legendl_file <- basename(leg_fll)
    attachments <- c(attachments, legendl = legendl_file)
  }

  # img_fldrs = list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
  # try(unlink(img_fldrs[grepl("image-", img_fldrs)],
  #            recursive = TRUE, force = TRUE),
  #     silent = TRUE)

  # ver = as.character(trunc(runif(1, 1000, 9999)))
  dep1 <- htmltools::htmlDependency(name = nm,
                                    version = "01",
                                    src = c(file = image_dir),
                                    attachment = attachments)
  deps <- list(dep1)

  sizing <- htmlwidgets::sizingPolicy(padding = 0, browser.fill = TRUE)

  # create widget
  htmlwidgets::createWidget(
    name = 'slideView',
    x,
    width = width,
    height = height,
    package = 'mapview',
    dependencies = deps,
    sizingPolicy = sizing
  )
}


#' Widget output function for use in Shiny
#'
#' @param outputId Output variable to read from
#' @param width,height the width and height of the canas element
#' (see \code{\link{shinyWidgetOutput}})
#'
#' @export
slideViewOutput <- function(outputId, width = '100%', height = '400px'){
  .Deprecated(new = "slideview::slideViewOutput", package = "mapview",
              old = "mapview::slideViewOutput")
  htmlwidgets::shinyWidgetOutput(outputId, 'slideView',
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
renderslideView <- function(expr, env = parent.frame(), quoted = FALSE) {
  .Deprecated(new = "slideview::renderslideView", package = "mapview",
              old = "mapview::renderslideView")
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, slideViewOutput, env, quoted = TRUE)
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
            .Deprecated(new = "slideview::slideview", package = "mapview",
                        old = "mapview::slideview")
            slideView(...)
          })

