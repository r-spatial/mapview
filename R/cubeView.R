#' View a cube of 3-dimensional data filled with points (voxels).
#'
#' A variation of Hovmoeller diagram: Each voxel is colored with a RGB-color value.
#'
#' The cube faces show a selectable layer of the data within the cube.
#'
#' Currently the visible layers are alterable by keys (not reliable within RStudio):
#'
#' z: PAGE_UP PAGE_DOWN
#'
#' x: INS DEL
#'
#' y: HOME END
#'
#'
#' Press and hold left mouse-button to rotate the cube.
#'
#' Press and hold right mouse-button to move the cube.
#'
#' Move mouse-wheel to zoom the cube.
#'
#' @author
#' Stephan Woellauer
#'
#' @import htmlwidgets
#'
#' @export
cubeView <- function(red, green, blue, x_size, y_size, z_size, width = NULL, height = NULL) {

  total_size <- x_size*y_size*z_size

  if(length(red)!=total_size) {
    stop("length of red vector not correct: ", length(red), " should be ", total_size)
  }
  if(length(green)!=total_size) {
    stop("length of green vector not correct", length(green), " should be ", total_size)
  }
  if(length(blue)!=total_size) {
    stop("length of blue vector not correct", length(blue), " should be ", total_size)
  }

  object_list <- list(red=red, green=green, blue=blue, x_size=x_size, y_size=y_size, z_size=z_size)

  # create widget
  htmlwidgets::createWidget(
    name = 'cubeView',
    x = object_list,
    width = width,
    height = height,
    package = 'mapview'
  )
}

#' Widget output function for use in Shiny
#'
#' @export
cubeViewOutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'cubeView', width, height, package = 'mapview')
}

#' Widget render function for use in Shiny
#'
#' @export
renderCubeView <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, cubeViewOutput, env, quoted = TRUE)
}
