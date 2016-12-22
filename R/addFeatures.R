### these functions call the appropriate leaflet::add* functions
### depending on geometry type. Additional parameters can be passed via ...

### Point Features ========================================================
addPointFeatures <- function(map,
                             data,
                             ...) {
  garnishMap(map, "addCircleMarkers", data = data, ...)
}

### Line Features =========================================================
addLineFeatures <- function(map,
                            data,
                            ...) {
  garnishMap(map, "addPolylines", data = data, ...)
}

### PolygonFeatures =======================================================
addPolygonFeatures <- function(map,
                               data,
                               ...) {
  garnishMap(map, "addPolygons", data = data, ...)
}

### addFeatures ===========================================================
### this is then the function to be called which internally decides which
### subfunction to use (to be expanded to basic classes - POINT etc)
addFeatures <- function(map,
                        data,
                        ...) {

  m <- switch(class(sf::st_geometry(data))[1],
              sfc_POINT = addPointFeatures(map, data, ...),
              sfc_MULTIPOINT = addPointFeatures(map, data, ...),
              sfc_LINESTRING = addLineFeatures(map, data, ...),
              sfc_MULTILINESTRING = addLineFeatures(map, data, ...),
              sfc_POLYGON = addPolygonFeatures(map, data, ...),
              sfc_MULTIPOLYGON = addPolygonFeatures(map, data, ...))

  return(m)

}
