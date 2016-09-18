sortableLayersControlDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "Sortable",
      '1.4.2',
      system.file("htmlwidgets/lib/sortable_layers_control", package = "mapview"),
      script = c("Sortable.js")
    ),
    htmltools::htmlDependency(
      "sortableLayersControl",
      '0.0.1',
      system.file("htmlwidgets/lib/sortable_layers_control", package = "mapview"),
      script = c("sortableLayersControl.js", "sortableLayersControl-src.js"),
      stylesheet = 'sortableLayersControl.css'
    )
  )
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

sortableLayersControl <- function(map, layer.names) {

  li <- do.call(paste0, lapply(seq(layer.names), function(i) {
    paste(
      "<li id=", layer.names[[i]],
      "><input type='checkbox' checked>",
      layer.names[[i]], "</li>", sep = "")
  }))

  inner_html <- paste(
    "<div class='layer block'><div class='layer title'><strong>Layers</strong>",
    "</div><ul id='layer_list' class='block__list block__list_words'>",
    li,
    "</ul></div>")

  map$dependencies <- c(map$dependencies, sortableLayersControlDependencies())
  leaflet::invokeMethod(map, leaflet:::getMapData(map), 'sortableLayersControl',
                        inner_html)

}
