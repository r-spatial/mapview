
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
