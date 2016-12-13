### these functions call the appropriate leaflet::add* functions
### depending on geometry type. Additional parameters can be passed via ...

### Point Features ========================================================
addPointFeatures <- function(map,
                             x,
                             ...) {
  leaflet::addCircleMarkers(map = map,
                            data = x,
                            ...)
}

### Line Features =========================================================
addLineFeatures <- function(map,
                            x,
                            ...) {
  leaflet::addPolylines(map = map,
                        data = x,
                        ...)
}

### PolygonFeatures =======================================================
addPolygonFeatures <- function(map,
                               x,
                               ...) {
  leaflet::addPolygons(map = map,
                       data = x,
                       ...)
}

### addFeatures ===========================================================
### this is then the function to be called which internally decides which
### subfunction to use (to be expanded to basic classes - POINT etc)
addFeatures <- function(map,
                        x,
                        ...) {

  m <- switch(class(sf::st_geometry(x))[1],
              sfc_POINT = addPointFeatures(map, x, ...),
              sfc_MULTIPOINT = addPointFeatures(map, x, ...),
              sfc_LINESTRING = addLineFeatures(map, x, ...),
              sfc_MULTILINESTRING = addLineFeatures(map, x, ...),
              sfc_POLYGON = addPolygonFeatures(map, x, ...),
              sfc_MULTIPOLYGON = addPolygonFeatures(map, x, ...))

  return(m)

}
