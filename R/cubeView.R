#' View a RasterStack or RasterBrick as 3-dimensional data cube.
#'
#' @param x a RasterStack or RasterBrick
#' @param at the breakpoints used for the visualisation. See
#' \code{\link{levelplot}} for details.
#' @param col.regions color (palette).See \code{\link{levelplot}} for details.
#' @param na.color color for missing values.
#' @param legend logical. Whether to plot a legend.
#' @param ... currently not used.
#'
#' @export cubeView
#' @name cubeView

cubeView <- function(x,
                     at,
                     col.regions = mapviewGetOption("raster.palette"),
                     na.color = mapviewGetOption("na.color"),
                     legend = TRUE) {
  .Defunct(new = "cubeview::cubeView", package = "mapview")
}

#' Widget output function for use in Shiny
#'
#' @param outputId Output variable to read from
#' @param width,height the width and height of the map
#' (see \code{\link{shinyWidgetOutput}})
#'
#' @export
cubeViewOutput <- function(outputId, width = '100%', height = '400px'){
  .Defunct(new = "cubeview::cubeViewOutput", package = "mapview")
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
  .Defunct(new = "cubeview::renderCubeView", package = "mapview")
}


## cubeview ===============================================================
#' @describeIn cubeView alias for ease of typing
#' @aliases cubeview
#' @export cubeview

cubeview <-  function(...) {
  .Defunct(new = "cubeview::cubeview", package = "mapview")
}

