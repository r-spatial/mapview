# #' @export
# #'
# print.mapview <- function(x, ...) {
#   htmlwidgets:::print.htmlwidget(mapview2leaflet(x), ...)
# }



### mapview to leaflet ----------------------------------------------------
mapview2leaflet <- function(x) {
  slot(x, "map")
}


### mapview simple class
getSimpleClass <- function(obj) {
  if (class(obj) %in% c("RasterLayer", "RasterStack",
                      "RasterBrick", "Satellite",
                      "SpatialGridDataFrame",
                      "SpatialPixelsDataFrame")) "rst" else "vec"
}


### labels
makeLabels <- function(col) {
  if (inherits(col, "POINT")) {
    lab <- "1"
  } else if (inherits(col, "MULTIPOINT")) {
    lab <- as.character(seq(nrow(col)))
  } else lab <- as.character(col)
}


### higlight options
mapviewHighlightOptions <- function(stroke = TRUE,
                                    color = "cyan",
                                    weight = 4,
                                    opacity = 1,
                                    fill = TRUE,
                                    fillColor = NULL,
                                    fillOpacity = 0,
                                    dashArray = NULL,
                                    bringToFront = TRUE,
                                    sendToBack = TRUE) {

  if (length(fillColor) != 1) fillColor <- color

  return(
    list(
      stroke = stroke,
      color = color,
      weight = weight,
      opacity = opacity,
      fill = fill,
      fillColor = fillColor,
      fillOpacity = fillOpacity,
      dashArray = dashArray,
      bringToFront = bringToFront,
      sendToBack = sendToBack
    )
  )
}

### decorateMap
# decorateMap <- function(map, ext, layer.name, ...) {
#
#   m <- garnishMap(map,
#                   addMouseCoordinates,
#                   addScaleBar,
#                     position = "bottomleft",
#                   addHomeButton,
#                     ext = ext,
#                     layer.name = layer.name)
#
# }


### getFeatureIds
getFeatureIds <- function(att_data) {
  if (inherits(att_data, "Spatial")) {
    ids <- row.names(att_data)
  }

  return(ids)
}


### createExtent
createExtent <- function(x, offset = 0.005) {

  if (inherits(x, "Spatial")) {
    if (length(x) > 1) {
      ext <- raster::extent(raster::projectExtent(x, crs = llcrs))
    } else {
      ext <- raster::extent(raster::xmin(x) - offset,
                            raster::xmax(x) + offset,
                            raster::ymin(x) - offset,
                            raster::ymax(x) + offset)
    }
  } else if (inherits(x, "sfc") | inherits(x, "sf")) {
    bb <- sf::st_bbox(x)
    ext <- raster::extent(bb[1] - offset,
                          bb[3] + offset,
                          bb[2] - offset,
                          bb[4] + offset)
  }

  return(ext)

}


isSingleFeature <- function(x) {
  #stopifnot(inherits(x, "sfg"))
  if (inherits(x, "POINT") |
      inherits(x, "LINESTRING") |
      inherits(x, "POLYGON")) {
    TRUE
  } else if (inherits(x, "MULTIPOINT") |
             inherits(x, "MULTILINESTRING") |
             inherits(x, "MULTIPOLYGON")) {
    FALSE
  } else FALSE
}

### burst
# burst <- function(x, zcol, ...) {
#
#   lst <- lapply(seq(zcol), function(i) {
#     if (!is.factor(x@data[, zcol[i]])) {
#       x@data[, zcol[i]] <- as.factor(x@data[, zcol[i]])
#     }
#     f <- x@data[, zcol[i]]
#     ls_out <- split(x[, zcol[i]], f, ...)
#     names(ls_out) <- paste(zcol[i], names(ls_out), sep = "_")
#     return(ls_out)
#   })
#
#   names(lst) <- zcol
#
#   if (length(lst) > 1) {
#     ls <- lst[[1]]
#     for (i in 2:length(lst)) ls <- append(ls, lst[[i]])
#   } else {
#     ls <- lst[[1]]
#   }
#
#   return(ls)
# }
