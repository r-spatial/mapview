#' @export
mapview.print <- function(x, ...) {
  htmlwidgets:::print.htmlwidget(mapview2leaflet(x), ...)
}

#' @export
#'
knit_print.mapview <- function(x, ...) {
  htmlwidgets:::knit_print.htmlwidget(mapview2leaflet(x), ...)
}


#' @export
#'
renderMapview <- function (expr, env = parent.frame(), quoted = FALSE) {
  expr <- mapview2leaflet(expr)
  if (!quoted)
    expr = substitute(expr)
  htmlwidgets::shinyRenderWidget(expr, leafletOutput, env,
                                 quoted = TRUE)
}

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
