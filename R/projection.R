## the two crs we use
wmcrs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
llcrs <- "+proj=longlat +datum=WGS84 +no_defs"

non_proj_warning <-
  "supplied layer has no projection information and is shown without background map"

wrong_proj_warning <-
  paste0("projection of supplied layer is not leaflet conform.", "\n",
         "  projecting to '", llcrs, "'")

# Check and potentially adjust projection of objects to be rendered =======
checkAdjustProjection <- function(x, method = "bilinear") {

  x <- switch(class(x)[1],
              "RasterLayer" = rasterCheckAdjustProjection(x, method),
              "RasterStack" = rasterCheckAdjustProjection(x, method),
              "RasterBrick" = rasterCheckAdjustProjection(x, method),
              "SpatialPointsDataFrame" = spCheckAdjustProjection(x),
              "SpatialPolygonsDataFrame" = spCheckAdjustProjection(x),
              "SpatialLinesDataFrame" = spCheckAdjustProjection(x),
              "SpatialPoints" = spCheckAdjustProjection(x),
              "SpatialPolygons" = spCheckAdjustProjection(x),
              "SpatialLines" = spCheckAdjustProjection(x),
              "sf" = sfCheckAdjustProjection(x),
              "XY" = sfCheckAdjustProjection(x),
              "sfc_POINT" = sfCheckAdjustProjection(x),
              "sfc_MULTIPOINT" = sfCheckAdjustProjection(x),
              "sfc_LINESTRING" = sfCheckAdjustProjection(x),
              "sfc_MULTILINESTRING" = sfCheckAdjustProjection(x),
              "sfc_POLYGON" = sfCheckAdjustProjection(x),
              "sfc_MULTIPOLYGON" = sfCheckAdjustProjection(x),
              "sfc_GEOMETRY" = sfCheckAdjustProjection(x),
              "sfc_GEOMETRYCOLLECTION" = sfCheckAdjustProjection(x))

  return(x)

}
#
#   if (class(x)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
#     x <- rasterCheckAdjustProjection(x)
#   } else if (class(x)[1] %in% c("SpatialPointsDataFrame",
#                                 "SpatialPolygonsDataFrame",
#                                 "SpatialLinesDataFrame",
#                                 "SpatialPoints",
#                                 "SpatialPolygons",
#                                 "SpatialLines")) {
#     x <- spCheckAdjustProjection(x)
#   }
#
#   return(x)
# }


# Project Raster* objects for mapView =====================================
rasterCheckAdjustProjection <- function(x, method) {

  is.fact <- raster::is.factor(x)[1]

  if (is.na(raster::projection(x))) {
    warning(non_proj_warning)
    raster::extent(x) <- scaleExtent(x)
    raster::projection(x) <- llcrs
  } else if (is.fact) {
    att = x@data@attributes
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(wmcrs)),
      method = "ngb")
    x <- raster::as.factor(x)
    x@data@attributes = att
  } else {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(wmcrs)),
      method = method)
  }

  return(x)

}


# Project stars* objects for mapView =====================================
starsCheckAdjustProjection <- function(x, method) {

  # is.fact <- raster::is.factor(x)[1]

  # if (is.na(raster::projection(x))) {
  #   warning(non_proj_warning)
  #   raster::extent(x) <- scaleExtent(x)
  #   raster::projection(x) <- llcrs
  # } else if (is.fact) {
  #   x <- raster::projectRaster(
  #     x, raster::projectExtent(x, crs = sp::CRS(wmcrs)),
  #     method = "ngb")
  #   x <- raster::as.factor(x)
  # } else {
  x <- sf::st_transform(
    x,
    crs = llcrs
  )
  # }

  return(x)

}


# Check and potentially adjust projection of sf objects ===================
sfCheckAdjustProjection <- function(x) {

  if (is.na(sf::st_crs(x))) {
    return(x) # warning(non_proj_warning)
  } else { #if (!validLongLat(sf::st_crs(x)$proj4string)) {
    x <- sf::st_transform(x, llcrs)
  }

  return(x)

}


# Check and potentially adjust projection of Spatial* objects =============
spCheckAdjustProjection <- function(x) {

  if (is.na(raster::projection(x))) {
    warning(non_proj_warning)
    if (class(x)[1] %in% c("SpatialPointsDataFrame", "SpatialPoints")) {
      methods::slot(x, "coords") <- scaleCoordinates(coordinates(x)[, 1],
                                                     coordinates(x)[, 2])
    } else if (class(x)[1] %in% c("SpatialPolygonsDataFrame",
                                  "SpatialPolygons")) {
      x <- scalePolygonsCoordinates(x)
    } else if (class(x)[1] %in% c("SpatialLinesDataFrame",
                                  "SpatialLines")) {
      x <- scaleLinesCoordinates(x)
    }

    raster::projection(x) <- llcrs

  } else if (!identical(raster::projection(x), llcrs)) {
    x <- sp::spTransform(x, CRSobj = llcrs)
  }

  return(x)

}

# Check projection of objects according to their keywords =================
# validLongLat <- function (p4s) {
#   proj <- datum <- nodefs <- FALSE
#   allWGS84 <- c("+init=epsg:4326", "+proj=longlat", "+datum=WGS84",
#                 "+no_defs", "+ellps=WGS84", "+towgs84=0,0,0")
#
#   p4s_splt = strsplit(p4s, " ")[[1]]
#
#   for (comp in allWGS84) {
#     if (comp %in% p4s_splt) {
#       if (comp == "+init=epsg:4326") {
#         proj <- datum <- nodefs <- TRUE
#       }
#       if (comp == "+proj=longlat") {
#         proj <- TRUE
#       }
#       if (comp == "+no_defs") {
#         nodefs <- TRUE
#       }
#       if (comp == "+datum=WGS84") {
#         datum <- TRUE
#       }
#     }
#   }
#   if (proj & datum & nodefs) {
#     return(TRUE)
#   } else {
#     warning(wrong_proj_warning)
#     return(FALSE)
#   }
# }
