#' @export
#'
print.mapview <- function(x, ...) {
  htmlwidgets:::print.htmlwidget(mapview2leaflet(x), ...)
}



### mapview to leaflet ----------------------------------------------------
mapview2leaflet <- function(x) {
  slot(x, "map")
}


### mapview simple class
getSimpleClass <- function(obj) {
  if (class(obj) %in% c("RasterLayer", "RasterStack",
                      "RasterBrick", "Satellite",
                      "SpatialGridDataFrame",
                      "SpatialPixelsDataFrame")) "rst" else "vec"
}


### labels
makeLabels <- function(col) {
  as.character(col)
}
