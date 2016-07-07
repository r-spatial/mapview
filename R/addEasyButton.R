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

#
# # ' Add full screen control to map
# # '
# # ' Add full screen control (https://github.com/brunob/leaflet.fullscreen)
# # ' @param map the map to add the tile layer to
# # ' @return modified map object
# # '
# # ' @examples
# # ' leaflet() %>%
# # '   addControlFullScreen()
# # ' @export
# addEasyButton <- function(
#   map
# ) {
#   map$dependencies <- c(map$dependencies, leafletFullScreenDependencies())
#   invokeMethod(map, leaflet::getMapData(map), 'addControlFullScreen')
# }

addHomeButton <- function(map, ext, layer.name) {

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
  invokeMethod(map, leaflet::getMapData(map), 'addHomeButton',
               ext@xmin, ext@ymin, ext@xmax, ext@ymax, label, txt)

}


# addEasyButton = function(map, xmin, ymin, xmax, ymax, label, icon) {
#
#   map$dependencies <- c(map$dependencies, leafletEasyButtonDependencies())
#   invokeMethod(map, leaflet::getMapData(map), 'addEasyButton',
#                xmin, ymin, xmax, ymax, label, icon)
# }


removeHomeButton <- function(map) {
  invokeMethod(map, NULL, 'removeHomeButton')
}

