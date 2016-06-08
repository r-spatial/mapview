#' Print functions for mapview objects used in knitr
#'
#' @param x A mapview object
#' @param ... further arguments passed on to \code{\link[knitr]{knit_print}}
#'
#' @export
#'
knit_print.mapview <- function(x, ...) {
  knitr::knit_print(mapview2leaflet(x), ...)
}


#' Render a mapview widget in shiny
#'
#' @param expr An expression that generates an HTML widget
#' @param env The environment in which to evaluate expr
#' @param quoted Is expr a quoted expression (with quote())?
#' This is useful if you want to save an expression in a variable
#'
#' @export
#'
renderMapview <- function (expr, env = parent.frame(), quoted = FALSE) {
  expr <- mapview2leaflet(expr)
  if (!quoted)
    expr = substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, leafletOutput, env,
                                 quoted = TRUE)
}

#' Create a mapview UI element for use with shiny
#'
#' @param outputId Output variable to read from
#' @param width,height the width and height of the map
#' (see \code{\link{shinyWidgetOutput}})
#'
#' @export
#'
mapviewOutput <- function (outputId, width = "100%", height = 400) {
  htmlwidgets::shinyWidgetOutput(outputId, "leaflet", width,
                                 height, "leaflet")
}

# #' @export
# #'
# prepare_evaluate_output.mapview <- function(output, ...) {
#   widget <- knitr::knit_print(mapview2leaflet(output))
#   meta <- attr(widget, "knit_meta")
#   asis <- knitr::asis_output(c(widget))
#   annotated <- html_notebook_annotated_output(asis, "htmlwidget", meta)
#   attr(annotated, "knit_meta") <- meta
#   annotated
# }
