#' @export
#'
print.mapview <- function(x, ...) {
  htmlwidgets:::print.htmlwidget(mapview2leaflet(x), ...)
}



### mapview to leaflet ----------------------------------------------------
mapview2leaflet <- function(x) {
  x@map
}


### mapview simple class
getSimpleClass <- function(x) {
  if (class(x) %in% c("RasterLayer", "RasterStack",
                      "RasterBrick", "Satellite",
                      "SpatialGridDataFrame",
                      "SpatialPixelsDataFrame")) "rst" else "vec"
}


### labels
makeLabels <- function(column) {
  labs <- as.character(column)
}
