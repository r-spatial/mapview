#' <Add Title>
#'
#' <Add Description>
#'
#' @author
#' Stephan Woellauer
#'
#' @import htmlwidgets
#'
#' @export
cubeView <- function(message, width = NULL, height = NULL) {

  # forward options using x
  x = list(
    message = message
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'cubeView',
    x,
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
