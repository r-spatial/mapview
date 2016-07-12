leafletEasyButtonDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "Leaflet.EasyButton",
      '0.0.1',
      system.file("htmlwidgets/lib/Leaflet.EasyButton", package = "mapview"),
      script = c("home-button.js", 'easy-button-src.js'),
      stylesheet = 'home-button.css'
    ))
}



addHomeButton <- function(map, ext, layer.name = "layer") {

  if (class(extent) == "matrix") ext <- raster::extent(ext)
  label <- paste("Zoom to", layer.name)

  txt <- paste('<strong>', layer.name, '</strong>')

  # xmin = ext@xmin,
  # ymin = ext@ymin,
  # xmax = ext@xmax,
  # ymax = ext@ymax,
  # label = label,
  # icon = txt

  map$dependencies <- c(map$dependencies, leafletEasyButtonDependencies())
  leaflet::invokeMethod(map, leaflet:::getMapData(map), 'addHomeButton',
                        ext@xmin, ext@ymin, ext@xmax, ext@ymax, label, txt)

}


# addEasyButton = function(map, xmin, ymin, xmax, ymax, label, icon) {
#
#   map$dependencies <- c(map$dependencies, leafletEasyButtonDependencies())
#   invokeMethod(map, leaflet::getMapData(map), 'addEasyButton',
#                xmin, ymin, xmax, ymax, label, icon)
# }


removeHomeButton <- function(map) {
  leaflet::invokeMethod(map, NULL, 'removeHomeButton')
}

