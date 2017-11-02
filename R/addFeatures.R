#' Type agnositc version of \code{leaflet::add*} functions.
#'
#' @description
#' Add simple features geomertries from \code{\link[sf]{sf}}
#'
#' @param map A \code{leaflet} or \code{mapview} map.
#' @param data A \code{sf} object to be added to the \code{map}.
#' @param ... Further arguments passed to the respective \code{leaflet::add*}
#' functions. See \code{\link{addCircleMarkers}}, \code{\link{addPolylines}}
#' and \code{\link{addPolygons}}.
#'
#' @return
#' A leaflet \code{map} object.
#'
#' @examples
#' \dontrun{
#' leaflet() %>% addTiles() %>% addCircleMarkers(data = breweries)
#' leaflet() %>% addTiles() %>% addFeatures(data = breweries)
#'
#' leaflet() %>% addTiles() %>% addPolylines(data = atlStorms2005)
#' leaflet() %>% addTiles() %>% addFeatures(atlStorms2005)
#'
#' leaflet() %>% addTiles() %>% addPolygons(data = franconia)
#' leaflet() %>% addTiles() %>% addFeatures(franconia)
#' }
#'
#' @export addFeatures
#' @name addFeatures
#' @rdname addFeatures
addFeatures <- function(map,
                        data,
                        ...) {

  if (inherits(data, "Spatial")) data = sf::st_as_sf(data)

  switch(getSFClass(sf::st_geometry(data)),
         sfc_POINT           = addPointFeatures(map, data, ...),
         sfc_MULTIPOINT      = addPointFeatures(map, data, ...),
         sfc_LINESTRING      = addLineFeatures(map, data, ...),
         sfc_MULTILINESTRING = addLineFeatures(map, data, ...),
         sfc_POLYGON         = addPolygonFeatures(map, data, ...),
         sfc_MULTIPOLYGON    = addPolygonFeatures(map, data, ...),
         sfc_GEOMETRY        = addGeometry(map, data, ...),
         POINT               = addPointFeatures(map, data, ...),
         MULTIPOINT          = addPointFeatures(map, data, ...),
         LINESTRING          = addLineFeatures(map, data, ...),
         MULTILINESTRING     = addLineFeatures(map, data, ...),
         POLYGON             = addPolygonFeatures(map, data, ...),
         MULTIPOLYGON        = addPolygonFeatures(map, data, ...),
         GEOMETRY            = addGeometry(map, data, ...))

}




### these functions call the appropriate leaflet::add* functions
### depending on geometry type. Additional parameters can be passed via ...

mw = 800

### Point Features ========================================================
addPointFeatures <- function(map,
                             data,
                             ...) {
  garnishMap(map, leaflet::addCircleMarkers,
             data = sf::st_zm(sf::st_cast(data, "POINT")),
             popupOptions = popupOptions(maxWidth = mw,
                                         closeOnClick = TRUE),
             ...)
}

### Line Features =========================================================
addLineFeatures <- function(map,
                            data,
                            ...) {
  garnishMap(map, leaflet::addPolylines,
             data = sf::st_zm(data),
             popupOptions = popupOptions(maxWidth = mw,
                                         closeOnClick = TRUE),
             ...)
}

### PolygonFeatures =======================================================
addPolygonFeatures <- function(map,
                               data,
                               ...) {
  garnishMap(map, leaflet::addPolygons,
             data = sf::st_zm(data),
             popupOptions = popupOptions(maxWidth = mw,
                                         closeOnClick = TRUE),
             ...)
}

### GeometryCollections ===================================================
addGeometry = function(map,
                       data,
                       ...) {
  ls = list(...)
  if (!is.null(ls$label))
    label = split(ls$label, f = as.character(sf::st_dimension(data)))
  if (!is.null(ls$popup))
    popup = split(ls$popup, f = as.character(sf::st_dimension(data)))
  lst = split(data, f = as.character(sf::st_dimension(data)))
  for (i in 1:length(lst)) {
    ls$map = map
    ls$data = sf::st_cast(lst[[i]])
    if (!is.null(ls$label)) ls$label = label[[i]]
    if (!is.null(ls$popup)) ls$popup = popup[[i]]
    map = do.call(addFeatures, ls)
      # addFeatures(map,
      #                 data = sf::st_cast(lst[[i]]),
      #                 group = ls$group,
      #                 radius = ls$radius,
      #                 weight = ls$weight,
      #                 opacity = ls$opacity,
      #                 fillOpacity = ls$fillOpacity,
      #                 color = ls$color,
      #                 fillColor = ls$fillColor,
      #                 popup = ls$popup[[i]],
      #                 label = ls$label[[i]])
  }
  return(map)
}

