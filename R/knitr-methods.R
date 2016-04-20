
# .onLoad <- function(...) {
#
#   htmltools:::registerMethods(list(
#     c("knitr", "knit_print", "mapview")
#   ))
#
# }



#' @export
#'
knit_print.mapview <- function(x, ..., options = NULL) {
  knitr::knit_print(htmlwidgets:::toHTML(mapview2leaflet(x),
                                         standalone = FALSE,
                                         knitrOptions = options),
                    options = options,  ...)
}


#' @export
#'
renderMapview <- function (expr, env = parent.frame(), quoted = FALSE) {
  expr <- expr@map
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
