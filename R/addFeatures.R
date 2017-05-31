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
                       group,
                       ...) {
  lst = split(data, f = as.character(sf::st_dimension(data)))
  m = mapview(lst[[1]], layer.name = group)
  lst = lst[2:length(lst)]
  Reduce( "+", lapply(lst, mapview, layer.name = group, homebutton = FALSE), init = m)
}

### addFeatures ===========================================================
### this is then the function to be called which internally decides which
### subfunction to use
addFeatures <- function(map,
                        data,
                        ...) {

  m <- switch(getSFClass(data),
              sfc_POINT           = addPointFeatures(map, data, ...),
              sfc_MULTIPOINT      = addPointFeatures(map, data, ...),
              sfc_LINESTRING      = addLineFeatures(map, data, ...),
              sfc_MULTILINESTRING = addLineFeatures(map, data, ...),
              sfc_POLYGON         = addPolygonFeatures(map, data, ...),
              sfc_MULTIPOLYGON    = addPolygonFeatures(map, data, ...),
              sfc_GEOMETRY        = addGeometry(map, data, group, ...),
              POINT               = addPointFeatures(map, data, ...),
              MULTIPOINT          = addPointFeatures(map, data, ...),
              LINESTRING          = addLineFeatures(map, data, ...),
              MULTILINESTRING     = addLineFeatures(map, data, ...),
              POLYGON             = addPolygonFeatures(map, data, ...),
              MULTIPOLYGON        = addPolygonFeatures(map, data, ...),
              GEOMETRY            = addGeometry(map, data, ...))

  return(m)

}

