### these functions call the appropriate leaflet::add* functions
### depending on geometry type. Additional parameters can be passed via ...

### Point Features ========================================================
addPointFeatures <- function(map,
                             data,
                             ...) {
  garnishMap(map, leaflet::addCircleMarkers, data = data,
             popupOptions = popupOptions(maxWidth = 2000,
                                         closeOnClick = TRUE),
             ...)
}

### Line Features =========================================================
addLineFeatures <- function(map,
                            data,
                            ...) {
  garnishMap(map, leaflet::addPolylines, data = data,
             popupOptions = popupOptions(maxWidth = 2000,
                                         closeOnClick = TRUE),
             ...)
}

### PolygonFeatures =======================================================
addPolygonFeatures <- function(map,
                               data,
                               ...) {
  garnishMap(map, leaflet::addPolygons, data = data,
             popupOptions = popupOptions(maxWidth = 2000,
                                         closeOnClick = TRUE),
             ...)
}

### GeometryCollections ===================================================
addGeometry = function(map,
                       data,
                       ...) {
  ls = list(...)
  if (!is.null(ls$label))
    ls$label = split(ls$label, f = as.character(sf::st_dimension(data)))
  if (!is.null(ls$popup))
    ls$popup = split(ls$popup, f = as.character(sf::st_dimension(data)))
  lst = split(data, f = as.character(sf::st_dimension(data)))
  for (i in 1:length(lst)) {
    map = addFeatures(map,
                      data = lst[[i]],
                      group = ls$group,
                      radius = ls$radius,
                      weight = ls$weight,
                      opacity = ls$opacity,
                      fillOpacity = ls$fillOpacity,
                      color = ls$color,
                      fillColor = ls$fillColor,
                      popup = ls$popup[[i]],
                      label = ls$label[[i]])
  }
  return(map)
}

### addFeatures ===========================================================
### this is then the function to be called which internally decides which
### subfunction to use
addFeatures <- function(map,
                        data,
                        ...) {

  switch(getSFClass(sf::st_geometry(sf::st_cast(data))),
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

