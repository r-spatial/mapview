# #' @export
# #'
# print.mapview <- function(x, ...) {
#   htmlwidgets:::print.htmlwidget(mapview2leaflet(x), ...)
# }
isAvailableInLeaflet <- function() {
  return(
    list(
      lab = "label" %in% names(as.list(args(leaflet::addCircleMarkers))),
      scl = "addScaleBar" %in% ls(getNamespace("leaflet"))
    )
  )
}

# lab_avl <- isAvailableInLeaflet()$lab
# scl_avl <- isAvailableInLeaflet()$scl

warn <- paste("Feature labels on mouseover and 'addScaleBar' are not supported in the installed version of 'leaflet'.",
              "\nRun devtools::install_github('rstudio/leaflet') and re-install 'mapview' locally to enable these features.")




### mapview to leaflet ----------------------------------------------------
mapview2leaflet <- function(x) {
  methods::slot(x, "map")
}


### mapview simple class
getSimpleClass <- function(obj) {
  if (class(obj) %in% c("RasterLayer", "RasterStack",
                      "RasterBrick", "Satellite",
                      "SpatialGridDataFrame",
                      "SpatialPixelsDataFrame")) "rst" else "vec"
}


### labels
makeLabels <- function(x, zcol = NULL) {
  if (inherits(x, "XY")) {
    lab <- "1"
  } else if (inherits(x, "sfc")) {
    lab <- as.character(seq(length(x)))
  } else if (inherits(x, "sf") & is.null(zcol)) {
    lab <- as.character(seq(nrow(x)))
  } else lab <- as.character(as.data.frame(x)[, zcol])
  return(lab)
}


makeLabelsSP <- function(col) {
  as.character(col)
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
  if (inherits(att_data, "Spatial") | inherits(att_data, "sf")) {
    ids <- row.names(att_data)
  } else if (inherits(att_data, "sfc")) {
    ids <- seq(length(att_data))
  }

  return(ids)
}


### createExtent
createExtent <- function(x, offset = 0.005) {

  if (inherits(x, "Raster")) {
    # ext <- raster::extent(raster::xmin(x) - offset,
    #                       raster::xmax(x) + offset,
    #                       raster::ymin(x) - offset,
    #                       raster::ymax(x) + offset)
    ext <- raster::extent(
      raster::projectExtent(x, crs = llcrs))
  } else if (inherits(x, "Spatial")) {
    ext <- raster::extent(raster::xmin(x) - offset,
                          raster::xmax(x) + offset,
                          raster::ymin(x) - offset,
                          raster::ymax(x) + offset)
  } else if (inherits(x, "sfc") | inherits(x, "sf") | inherits(x, "XY")) {
    bb <- sf::st_bbox(x)
    ext <- raster::extent(bb[1] - offset,
                          bb[3] + offset,
                          bb[2] - offset,
                          bb[4] + offset)
  }

  return(ext)

}


isMultiFeature <- function(x) {
  #stopifnot(inherits(x, "sfg"))
  if (inherits(x, "POINT") |
      inherits(x, "LINESTRING") |
      inherits(x, "POLYGON")) {
    FALSE
  } else if (inherits(x, "MULTIPOINT") |
             inherits(x, "MULTILINESTRING") |
             inherits(x, "MULTIPOLYGON")) {
    TRUE
  } else FALSE
}


getGeometryType <- function(x) {
  # sf
  g <- sf::st_geometry(x)
  if (inherits(g, "POINT") |
      inherits(g, "MULTIPOINT") |
      inherits(g, "sfc_POINT") |
      inherits(g, "sfc_MULTIPOINT")) type <- "pt"
  if (inherits(g, "LINESTRING") |
      inherits(g, "MULTILINESTRING") |
      inherits(g, "sfc_LINESTRING") |
      inherits(g, "sfc_MULTILINESTRING")) type <- "ln"
  if (inherits(g, "POLYGON") |
      inherits(g, "MULTIPOLYGON") |
      inherits(g, "sfc_POLYGON") |
      inherits(g, "sfc_MULTIPOLYGON")) type <- "pl"
  return(type)
}


getMaxFeatures <- function(x) {
  switch(getGeometryType(x),
         "pt" = 40000,
         "ln" = 100000,
         "pl" = 100000)
}


lineWidth <- function(x) {
  switch(getGeometryType(x),
         "pt" = 3,
         "ln" = 2,
         "pl" = 1)
}


basemaps <- function(colors) {
  ml <- mean(as.numeric(sapply(colors, luminence)))
  if (ml > 0.8) mapviewGetOption("basemaps")[c(2, 1, 3:5)] else
    mapviewGetOption("basemaps")
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
