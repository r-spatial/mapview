#' View a cube of 3-dimensional data filled with points (voxels).
#'
#' A variation of Hovmoeller diagram: Each voxel is colored with a RGB-color (or grey) value.
#'
#' @param x_size integer. size of x-dimension
#'
#' @param y_size integer. size of y-dimension
#'
#' @param z_size integer. size of z-dimension
#'
#' @param grey optional integer vector with 0 <= value <= 255.
#'
#' @param red optional integer vector with 0 <= value <= 255.
#'
#' @param green optional integer vector with 0 <= value <= 255.
#'
#' @param blue optional integer vector with 0 <= value <= 255.
#'
#' @details
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
#' The color resp. grey vectors contain sequentially values of each voxel. So each vector is length == x_size * y_size * z_size.
#' Color component values overwrite grey values.
#'
#' Sequence of coordinates (x,y,z) for values in vectors:
#'
#' (1,1,1), (2,1,1), (3,1,1), ... (1,2,1), (2,2,1), (3,2,1), ... (1,1,2), (2,1,2), (3,1,2), ...
#'
#'
#' @author
#' Stephan Woellauer
#'
#' @import htmlwidgets
#'
#' @export
cubeView <- function(grey=NULL, red=NULL, green=NULL, blue=NULL, x_size, y_size, z_size, width = NULL, height = NULL) {

  total_size <- x_size*y_size*z_size

  object_list <- list(x_size=x_size, y_size=y_size, z_size=z_size)

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

  # create widget
  htmlwidgets::createWidget(
    name = 'cubeView',
    x = object_list,
    width = width,
    height = height,
    package = 'mapview',
    sizingPolicy = htmlwidgets::sizingPolicy(padding = 0, browser.fill = TRUE)
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
