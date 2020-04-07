#' Garnish/decorate leaflet or mapview maps.
#'
#' @description
#' This function provides a versatile interface to add components to a
#' leaflet or mapview map. It takes functions such as "addMouseCoordinates"
#' or \code{\link{addLayersControl}} and their respective arguments and adds
#' them to the map. Arguments must be named. Functions can be plain or
#' character strings.
#'
#' @param map a mapview or leaflet object.
#' @param ... functions and their arguments to add things to a map.
#'
#' @examples
#' library(leaflet)
#' library(leafem)
#' library(leafpop)
#'
#' m <- leaflet() %>% addProviderTiles("OpenStreetMap")
#' garnishMap(m, leafem::addMouseCoordinates, style = "basic")
#'
#' ## add more than one with named argument
#' library(raster)
#'
#' m1 <- garnishMap(m, leafem::addMouseCoordinates, leafem::addHomeButton,
#'                  ext = extent(breweries))
#' m1
#'
#' ## even more flexible
#' m2 <- garnishMap(m1, addPolygons, data = franconia,
#'                  popup = leafpop::popupTable(franconia),
#'                  fillOpacity = 0.8, color = "black", fillColor = "#BEBEBE")
#' garnishMap(m2, addCircleMarkers, data = breweries)
#'
#' @export garnishMap
#' @name garnishMap
#' @rdname garnishMap
#' @aliases garnishMap
garnishMap <- function(map, ...) {
  .Defunct(new = "leafem::garnishMap", package = "mapview")
}

### decorateMap lets you pass lists of functions with respective lists of
### named lists of arguments as in
### decorateMap(map, list(addCircleMarkers), list(list(data = breweries91)))
decorateMap <- function(map, funs, args) {
  for (i in seq(funs)) {
    map <- do.call(leafem::garnishMap, c(list(map), funs[[i]], args[[i]]))
  }
  return(map)
}
