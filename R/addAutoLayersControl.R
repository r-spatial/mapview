
addAutoLayersControl <- function(map, baselayers, overlays) {
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))

  map$dependencies <- c(map$dependencies, leafletAutoLayersDependencies())
  leaflet::invokeMethod(map, leaflet:::getMapData(map), 'addAutoLayersControl',
                        baselayers, overlays)

}


removeAutoLayersControl <- function(map) {
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))
  leaflet::invokeMethod(map, NULL, 'removeAutoLayersControl')
}




leafletAutoLayersDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "Leaflet.AutoLayers",
      '0.0.1',
      system.file("htmlwidgets/lib/Leaflet.AutoLayers", package = "mapview"),
      script = c("leaflet-autolayers.js", 'leaflet-autolayers-binding.js'),
      stylesheet = 'leaflet.auto-layers.css'
    ))
}


