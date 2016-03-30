#' @export
#'
print.mapview <- function(x, ...) {

  htmlwidgets:::print.htmlwidget(mapview2leaflet(x), ...)

}



### mapview to leaflet ----------------------------------------------------
mapview2leaflet <- function(x) {
  x@map
}
