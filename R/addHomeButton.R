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
#' @param add logical. Whether to add the button to the map (mainly for internal use).
#'
#' @examples
#' \dontrun{
#' library(mapview)
#' library(raster)
#'
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
                          position = 'bottomleft', add = TRUE) {
  if (inherits(map, "mapview")) map <- mapview2leaflet(map)
  stopifnot(inherits(map, "leaflet"))

  hb <- try(getCallEntryFromMap(map, "addHomeButton"), silent = TRUE)
  if (!inherits(hb, "try-error") & length(hb) == 1) {
    ext_coords <- unlist(map$x$calls[[hb]][["args"]][1:4])
    ext_map <- raster::extent(ext_coords[1],
                              ext_coords[3],
                              ext_coords[2],
                              ext_coords[4])
    if (identical(ext, ext_map)) add = FALSE
  }

  if (add) {
    if (class(extent) == "matrix") ext <- raster::extent(ext)
    label <- paste("Zoom to", layer.name)

    txt <- paste('<strong>', layer.name, '</strong>')

    map$dependencies <- c(map$dependencies, leafletHomeButtonDependencies())
    leaflet::invokeMethod(map, leaflet::getMapData(map), 'addHomeButton',
                          ext@xmin, ext@ymin, ext@xmax, ext@ymax, label, txt,
                          position)
  }

  else map

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




leafletHomeButtonDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "HomeButton",
      '0.0.1',
      system.file("htmlwidgets/lib/HomeButton", package = "mapview"),
      script = c("home-button.js", 'easy-button-src.js'),
      stylesheet = 'home-button.css'
    ))
}


