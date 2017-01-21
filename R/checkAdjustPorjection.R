# Check and potentially adjust projection of objects to be rendered =======
checkAdjustProjection <- function(x) {

  x <- switch(class(x)[1],
              "RasterLayer" = rasterCheckAdjustProjection(x),
              "RasterStack" = rasterCheckAdjustProjection(x),
              "RasterBrick" = rasterCheckAdjustProjection(x),
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
              "sfc_MULTIPOLYGON" = sfCheckAdjustProjection(x))

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
rasterCheckAdjustProjection <- function(x) {

  is.fact <- raster::is.factor(x)[1]

  non_proj_waning <-
    paste("supplied", class(x)[1], "has no projection information!", "\n",
          "scaling coordinates and showing layer without background map")

  if (is.na(raster::projection(x))) {
    warning(non_proj_waning)
    raster::extent(x) <- scaleExtent(x)
    raster::projection(x) <- llcrs
  } else if (is.fact) {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(wmcrs)),
      method = "ngb")
    x <- raster::as.factor(x)
  } else {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(wmcrs)),
      method = "bilinear")
  }

  return(x)

}


# Check and potentially adjust projection of Spatial* objects =============
spCheckAdjustProjection <- function(x) {

  non_proj_waning <-
    paste("supplied", class(x)[1], "has no projection information!", "\n",
          "scaling coordinates and showing layer without background map")

  if (is.na(raster::projection(x))) {
    warning(non_proj_waning)
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


# Check and potentially adjust projection of sf objects ===================
sfCheckAdjustProjection <- function(x) {

  non_proj_waning <-
    paste("supplied", class(x)[1], "has no projection information!", "\n",
          "scaling coordinates and showing layer without background map")

  if (is.na(sf::st_crs(x)$proj4string)) {
    warning(non_proj_waning)
    # if (class(x)[1] %in% c("SpatialPointsDataFrame", "SpatialPoints")) {
    #   methods::slot(x, "coords") <- scaleCoordinates(coordinates(x)[, 1],
    #                                                  coordinates(x)[, 2])
    # } else if (class(x)[1] %in% c("SpatialPolygonsDataFrame",
    #                               "SpatialPolygons")) {
    #   x <- scalePolygonsCoordinates(x)
    # } else if (class(x)[1] %in% c("SpatialLinesDataFrame",
    #                               "SpatialLines")) {
    #   x <- scaleLinesCoordinates(x)
    # }
    #
    # raster::projection(x) <- llcrs

  } else if (!identical(sf::st_crs(x)$proj4string, llcrs)) {
    x <- sf::st_transform(x, llcrs)
  }

  return(x)

}
