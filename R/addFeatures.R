### these functions call the appropriate leaflet::add* functions
### depending on geometry type. Additional parameters can be passed via ...

### Point Features ========================================================
addPointFeatures <- function(map,
                             data,
                             ...) {
  ls <- list(...)
  nms <- names(ls)[names(ls) != ""]
  args <- match.arg(nms, names(as.list(match.fun("addCircleMarkers"))),
                    several.ok = TRUE)
  do.call("addCircleMarkers", append(list(map, data = data), ls[args]))
  # leaflet::addCircleMarkers(map = map,
  #                           data = x,
  #                           ...)
}

### Line Features =========================================================
addLineFeatures <- function(map,
                            data,
                            ...) {
  ls <- list(...)
  nms <- names(ls)[names(ls) != ""]
  args <- match.arg(nms, names(as.list(match.fun("addPolylines"))),
                    several.ok = TRUE)
  do.call("addPolylines", append(list(map, data = data), ls[args]))
  # leaflet::addPolylines(map = map,
  #                       data = x,
  #                       args)
}

### PolygonFeatures =======================================================
addPolygonFeatures <- function(map,
                               data,
                               ...) {
  ls <- list(...)
  nms <- names(ls)[names(ls) != ""]
  args <- match.arg(nms, names(as.list(match.fun("addPolygons"))),
                    several.ok = TRUE)
  do.call("addPolygons", append(list(map, data = data), ls[args]))
  # leaflet::addPolygons(map = map,
  #                      data = x,
  #                      ...)
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
