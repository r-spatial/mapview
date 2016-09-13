#' Add a home button / zoom-to-layer button to a map.
#'
#' @description
#' This function adds a button to the map that enables zooming to a
#' provided \code{\link{extent}} / \code{\link{bbox}}.
#'
#' @param map a mapview or leaflet object.
#' @param ext the \code{\link{extent}} / \code{\link{bbox}} to zoom to.
#' @param layer.name the name of the layer to be zoomed to (or any character
#' string)
#' @param position the position of the button (one of 'topleft', 'topright',
#' 'bottomleft', 'bottomright'). Defaults to 'bottomright'.
#'
#' @examples
#' \dontrun{
#' m <- leaflet() %>% addTiles() %>% addCircleMarkers(data = breweries91) %>%
#'   addHomeButton(extent(breweries91), "breweries91")
#' m
#'
#' ## remove the button
#' m1 <- removeHomeButton(m)
#' m1
#'
#' addHomeButton(m1, extent(breweries91), "anynamewilldoactually", 'topleft')
#' }
#'
#'
#' @export addHomeButton
#' @name addHomeButton
#' @rdname addHomeButton
#' @aliases addHomeButton
addHomeButton <- function(map, ext, layer.name = "layer",
                          position = 'bottomright') {
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))

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
                        ext@xmin, ext@ymin, ext@xmax, ext@ymax, label, txt,
                        position)

}


#' Use removeHomeButton to remove home button
#'
#' @describeIn addHomeButton remove a homeButton from a map
#' @aliases removeHomeButton
#' @export removeHomeButton
removeHomeButton <- function(map) {
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))
  leaflet::invokeMethod(map, NULL, 'removeHomeButton')
}




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


