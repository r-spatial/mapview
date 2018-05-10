#' Method for printing mapview objects
#' @param x a mapview object
setMethod('print', signature(x = "mapview"),
          function(x) {
            #print.mapview(x)
            print(mapview2leaflet(x))
          }
)

#' Method for printing mapview objects (show)
#' @param object a mapview object
setMethod("show", signature(object = "mapview"),
          function(object)
          {
            print(object)
          }
)


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
renderMapview <- function(expr, env = parent.frame(), quoted = FALSE) {
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
