
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




### mapview to leaflet
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
    lab <- rownames(x)
  } else lab <- as.character(as.data.frame(x)[, zcol])
  return(lab)
}


makeLabelsSP <- function(col) {
  as.character(col)
}


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
createExtent <- function(x, offset = NULL) {

  if (inherits(x, "Extent")) {
    return(x)
  } else {
    if (inherits(x, "Raster")) {
      ext <- raster::extent(
        raster::projectExtent(x, crs = llcrs))
    } else if (inherits(x, "Spatial")) {
      ext <- raster::extent(raster::xmin(x),
                            raster::xmax(x),
                            raster::ymin(x),
                            raster::ymax(x))
    } else if (inherits(x, "sfc") | inherits(x, "sf") | inherits(x, "XY")) {
      bb <- sf::st_bbox(x)
      ext <- raster::extent(bb[1], bb[3], bb[2], bb[4])
    }

    if (is.null(offset)) {
      xxtend <- extendLimits(c(ext[1], ext[2]))
      yxtend <- extendLimits(c(ext[3], ext[4]))
      ext@xmin <- xxtend[1]
      ext@xmax <- xxtend[2]
      ext@ymin <- yxtend[1]
      ext@ymax <- yxtend[2]
    } else {
      ext@xmin <- ext@xmin - offset
      ext@xmax <- ext@xmax + offset
      ext@ymin <- ext@ymin - offset
      ext@ymax <- ext@ymax + offset
    }

    return(ext)
  }

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


### getSFClass
getSFClass <- function(x) {
  if (class(x)[1] == "XY") class(x)[2] else class(x)[1]
}


getGeometryType <- function(x) {
  # sf
  if (inherits(x, "Spatial")) x = sf::st_as_sfc(x)
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
  if (inherits(g, "sfc_GEOMETRY")) type <- "gc" #getGeometryType(sf::st_cast(g))
  return(type)
}


getMaxFeatures <- function(x) {
  switch(getGeometryType(x),
         "pt" = 40000,
         "ln" = 100000,
         "pl" = 100000,
         "gc" = 100000)
}


lineWidth <- function(x) {
  switch(getGeometryType(x),
         "pt" = 2,
         "ln" = 2,
         "pl" = 1,
         "gc" = 2)
}


regionOpacity <- function(x) {
  switch(getGeometryType(x),
         "pt" = 0.9,
         "ln" = 1,
         "pl" = 0.6,
         "gc" = 0.6)
}


basemaps <- function(colors) {
  ml <- mean(as.numeric(sapply(colors, luminence)))
  if (ml > 0.8) mapviewGetOption("basemaps")[c(2, 1, 3:5)] else
    mapviewGetOption("basemaps")
}


getProjection <- function(x) {

  if (inherits(x, c("Raster", "Spatial"))) {
    prj <- raster::projection(x)
  } else {
    prj <- sf::st_crs(x)[["proj4string"]]
  }

  return(prj)

}


createFileId <- function(ndigits = 6) {
  paste(sample(c(letters[1:6], 0:9), ndigits), collapse = "")
}


extendLimits <- function(lim, length = 1, prop = 0.07) {
  if(length(lim) != 2) stop("lim should be of length 2")
  if(lim[1] > lim[2]) lim <- rev(lim)
  if (!missing(length)) {
    prop <- (as.numeric(length) -
               as.numeric(diff(lim))) / (2 * as.numeric(diff(lim)))
  }
  if (lim[1] == lim[2])
    lim + 0.05 * c(-length, length)
  else {
    d <- diff(as.numeric(lim))
    lim + prop * d * c(-1, 1)
  }
}


circleRadius <- function(x, radius = 6, min.rad = 3, max.rad = 15) {

  if (is.character(radius)) {
    rad <- scales::rescale(as.numeric(x[[radius]]),
                           to = c(min.rad, max.rad))
    rad[is.na(rad)] = 1
  } else rad <- radius
  return(rad)
}


extentOverlap <- function(x, y) {
  if (!sum(lengths(sf::st_intersects(x, y))) == 0) TRUE else FALSE
}


makeLayerName = function(x, zcol, up = 3) {
  lnm = deparse(substitute(x, env = parent.frame(up)))
  if (is.null(zcol)) lnm else paste(lnm, zcol, sep = " - ")
}



makeListLayerNames = function(x, layer.name) {
  if (length(layer.name) == length(x)) {
    lnms = layer.name
  } else if (!is.null(names(x))) {
    lnms = names(x)
  } else {
    chr = gsub(utils::glob2rx("*list(*"), "", layer.name)
    chr = unlist(strsplit(x = gsub(")", "", chr), ","))
    lnms = gsub(" ", "", chr)
  }
  return(as.list(lnms))
}
