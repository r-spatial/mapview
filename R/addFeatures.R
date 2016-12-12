### these functions call the appropriate leaflet::add* functions
### depending on geometry type. Additional parameters can be passed via ...

### sfc_POINT =============================================================
add_sfc_POINT <- function(map,
                          x,
                          ...) {
  leaflet::addCircleMarkers(map = map,
                            data = x,
                            ...)
}

### sfc_MULTIPOINT ========================================================
add_sfc_MULTIPOINT <- function(map,
                               x,
                               ...) {
  leaflet::addCircleMarkers(map = map,
                            data = x,
                            ...)
}

### sfc_LINESTRING ========================================================
add_sfc_LINESTRING <- function(map,
                               x,
                               ...) {
  leaflet::addPolylines(map = map,
                        data = x,
                        ...)
}

### sfc_MULTILINESTRING ===================================================
add_sfc_MULTILINESTRING <- function(map,
                                    x,
                                    ...) {
  leaflet::addPolylines(map = map,
                        data = x,
                        ...)
}

### sfc_POLYGON ===========================================================
add_sfc_POLYGON <- function(map,
                               x,
                               ...) {
  leaflet::addPolygons(map = map,
                       data = x,
                       ...)
}

### sfc_MULTIPOLYGON ======================================================
add_sfc_MULTIPOLYGON <- function(map,
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
              sfc_POINT = add_sfc_POINT(map, x, ...),
              sfc_MULTIPOINT = add_sfc_MULTIPOINT(map, x, ...),
              sfc_LINESTRING = add_sfc_LINESTRING(map, x, ...),
              sfc_MULTILINESTRING = add_sfc_MULTILINESTRING(map, x, ...),
              sfc_POLYGON = add_sfc_POLYGON(map, x, ...),
              sfc_MULTIPOLYGON = add_sfc_MULTIPOLYGON(map, x, ...))

  return(m)

}
