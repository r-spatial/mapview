#' convenience functions for working with spatial objects and leaflet maps
#'
#' @author
#' Tim Appelhans
#'
#' @name mapControls
NULL

# query leaflet map for position of 'addLayersControl' entry --------------
#' @describeIn mapControls query leaflet map for position of 'addLayersControl' entry
#' @export getLayerControlEntriesFromMap
#'
#' @param map a leaflet map
getLayerControlEntriesFromMap <- function(map) {

  seq_along(map$x$calls)[sapply(map$x$calls,
                                FUN = function(X) "addLayersControl" %in% X)]

}

# get layer names of leaflet map ------------------------------------------
#' @describeIn mapControls get layer names of leaflet map
#' @export getLayerNamesFromMap
#'
getLayerNamesFromMap <- function(map) {

  len <- getLayerControlEntriesFromMap(map)
  len <- len[length(len)]
  if (length(len) != 0) map$x$calls[[len]]$args[[2]] else NULL

}


# update layer names of leaflet map ---------------------------------------
#' @describeIn mapControls update layer names of leaflet map
#' @export updateLayerControlNames
#'
#' @param map1 map to be modified
#' @param map2 map to get modification from
#'
updateLayerControlNames <- function(map1, map2) {
  len <- getLayerControlEntriesFromMap(map1)
  len <- len[length(len)]
  map1$x$calls[[len]]$args[[2]] <- c(getLayerNamesFromMap(map1),
                                     getLayerNamesFromMap(map2))
  return(map1)
}

# identify layers to be hidden from initial map rendering -----------------
#' @describeIn mapControls identify layers to be hidden from initial map rendering
#' @export layers2bHidden
#'
layers2bHidden <- function(map) {

  nms <- getLayerNamesFromMap(map)
  nms[-c(1)]

}



# get calls from a map ----------------------------------------------------
#' @describeIn mapControls get calls from a map
#' @export getMapCalls
#'
getMapCalls <- function(map) {
  map$x$calls
}



# append calls to a map ---------------------------------------------------
#' @describeIn mapControls append calls to a map
#' @export appendMapCallEntries
#'
appendMapCallEntries <- function(map1, map2) {
  ## base map controls
  ctrls1 <- getLayerControlEntriesFromMap(map1)
  ctrls2 <- getLayerControlEntriesFromMap(map2)
  bmaps1 <- map1$x$calls[[ctrls1[1]]]$args[[1]]
  bmaps2 <- map2$x$calls[[ctrls2[1]]]$args[[1]]
  bmaps <- c(bmaps1, bmaps2)[!duplicated(c(bmaps1, bmaps2))]

  ## layer controls
  lyrs1 <- getLayerNamesFromMap(map1)
  lyrs2 <- getLayerNamesFromMap(map2)
  lyrs <- c(lyrs1, lyrs2)
  dup <- duplicated(lyrs)
  lyrs[dup] <- paste0(lyrs[dup], ".2")

  ## merge
  mpcalls <- append(map1$x$calls, map2$x$calls)
  mpcalls <- mpcalls[!duplicated(mpcalls)]
  mpcalls[[ctrls1[1]]]$args[[1]] <- bmaps
  mpcalls[[ctrls1[1]]]$args[[2]] <- lyrs

  ind <- seq_along(mpcalls)[sapply(mpcalls,
                                  FUN = function(X) "addLayersControl" %in% X)]
  ind1 <- ind[1]
  ind2 <- ind[-1]
  try({
    mpcalls[[ind2]] <- mpcalls[[ind1]]
    mpcalls[[ind1]] <- NULL
  }, silent = TRUE)

  map1$x$calls <- mpcalls
  return(map1)
}



# remove duuplicated map calls --------------------------------------------
#' @describeIn mapControls remove duuplicated map calls
#' @export removeDuplicatedMapCalls
#'
removeDuplicatedMapCalls <- function(map) {
  ind <- anyDuplicated(map$x$calls)
  for (i in ind) map$x$calls[[ind]] <- NULL
  return(map)
}



wmcrs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
llcrs <- "+proj=longlat +datum=WGS84 +no_defs"


# project Raster* objects for mapView -------------------------------------
#' @describeIn mapControls check and potentially adjust projection of Raster* objects
#' @export rasterCheckAdjustProjection
#'
#' @param x a Raster* or Spatial* object
#' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
rasterCheckAdjustProjection <- function(x, maxpixels) {

  if (maxpixels < ncell(x)) {
    warning(paste("maximum number of pixels for Raster* viewing is",
                  maxpixels, "the supplied Raster* has", ncell(x), "\n",
                  "... decreasing Raster* resolution to", maxpixels, "pixels\n",
                  "to view full resolution adjust 'maxpixels = ...'"))
    x <- sampleRegular(x, maxpixels, asRaster = TRUE, useGDAL = TRUE)
  }
  is.fact <- raster::is.factor(x)[1]
  if (is.fact) {
    out_rst <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(wmcrs)),
      method = "ngb")
    out_rst <- raster::as.factor(out_rst)
  } else {
    out_rst <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(wmcrs)),
      method = "bilinear")
  }

  return(out_rst)

}


# initialise mapView base maps --------------------------------------------
#' @describeIn mapControls initialise mapView base maps
#' @export initBaseMaps
#'
#' @param map.types map types to be used as backgraound maps
initBaseMaps <- function(map.types) {
  ## create base map using specified map types
  if (missing(map.types)) map.types <- c("OpenStreetMap",
                                         "Esri.WorldImagery")
    m <- leaflet::leaflet()
    m <- leaflet::addProviderTiles(m, provider = map.types[1],
                                   group = map.types[1])
    if (length(map.types) > 1) {
      for (i in 2:length(map.types)) {
        m <- leaflet::addProviderTiles(m, provider = map.types[i],
                                       group = map.types[i])
      }
    }
  return(m)
}


# initialise mapView map --------------------------------------------------
#' @describeIn mapControls initialise mapView map
#' @export initMap
#'
#' @param proj4str \code{\link{proj4string}} of the spatial objects to be viewed
initMap <- function(map, map.types, proj4str) {

  if (missing(map.types)) map.types <- c("OpenStreetMap",
                                         "Esri.WorldImagery")
  if (missing(map)) map <- NULL
  if (missing(proj4str)) proj4str <- NA
  ## create base map using specified map types
  if (is.null(map)) {
    if (is.na(proj4str)) {
      m <- leaflet::leaflet()
    } else {
      m <- initBaseMaps(map.types)
    }
  } else {
    m <- map
  }
  return(m)
}


# scale coordinates for unprojected spatial objects -----------------------
#' @describeIn mapControls scale coordinates for unprojected spatial objects
#' @export scaleCoordinates
#'
#' @param x.coords vector of x coordinates
#' @param y.coords vector of y coordinates
scaleCoordinates <- function(x.coords, y.coords) {

  ratio <- diff(range(y.coords)) / diff(range(x.coords))
  x_mn <- x.coords - min(x.coords, na.rm = TRUE)
  x_sc <- x_mn / max(x_mn, na.rm = TRUE)
  y_mn <- y.coords - min(y.coords, na.rm = TRUE)
  y_sc <- y_mn / max(y_mn, na.rm = TRUE) * ratio

  return(cbind(x_sc, y_sc))

}


# scale unprojected SpatialPolygons* objects ------------------------------
#' @describeIn mapControls scale unprojected SpatialPolygons* objects
#' @export scalePolygonsCoordinates
#'
scalePolygonsCoordinates <- function(x) {

  coord_lst <- lapply(slot(x, "polygons"), function(x) {
    lapply(slot(x, "Polygons"), function(y) slot(y, "coords"))
  })

  xcoords <- do.call("c", do.call("c", lapply(seq(coord_lst), function(i) {
    lapply(seq(coord_lst[[i]]), function(j) {
      coord_lst[[i]][[j]][, 1]
    })
  })))

  ycoords <- do.call("c", do.call("c", lapply(seq(coord_lst), function(i) {
    lapply(seq(coord_lst[[i]]), function(j) {
      coord_lst[[i]][[j]][, 2]
    })
  })))

  ratio <- diff(range(ycoords)) / diff(range(xcoords))

  x_mn <- min(xcoords, na.rm = TRUE)
  x_mx <- max(xcoords - min(xcoords, na.rm = TRUE), na.rm = TRUE)

  y_mn <- min(ycoords, na.rm = TRUE)
  y_mx <- max(ycoords - min(ycoords, na.rm = TRUE), na.rm = TRUE)

  for (j in seq(coord_lst)) {
    for (h in seq(coord_lst[[j]])) {
      slot(x@polygons[[j]]@Polygons[[h]], "coords") <-
        cbind((sp::coordinates(x@polygons[[j]]@Polygons[[h]])[, 1] - x_mn) / x_mx,
              (sp::coordinates(x@polygons[[j]]@Polygons[[h]])[, 2] - y_mn) / y_mx)
    }
  }

  return(x)

}


# scale unprojected SpatialLines* objects ------------------------------
#' @describeIn mapControls scale unprojected SpatialLines* objects
#' @export scaleLinesCoordinates
#'
scaleLinesCoordinates <- function(x) {

  coord_lst <- lapply(slot(x, "lines"), function(x) {
    lapply(slot(x, "Lines"), function(y) slot(y, "coords"))
  })

  xcoords <- do.call("c", do.call("c", lapply(seq(coord_lst), function(i) {
    lapply(seq(coord_lst[[i]]), function(j) {
      coord_lst[[i]][[j]][, 1]
    })
  })))

  ycoords <- do.call("c", do.call("c", lapply(seq(coord_lst), function(i) {
    lapply(seq(coord_lst[[i]]), function(j) {
      coord_lst[[i]][[j]][, 2]
    })
  })))

  ratio <- diff(range(ycoords)) / diff(range(xcoords))

  x_mn <- min(xcoords, na.rm = TRUE)
  x_mx <- max(xcoords - min(xcoords, na.rm = TRUE), na.rm = TRUE)

  y_mn <- min(ycoords, na.rm = TRUE)
  y_mx <- max(ycoords - min(ycoords, na.rm = TRUE), na.rm = TRUE)

  for (j in seq(coord_lst)) {
    for (h in seq(coord_lst[[j]])) {
      slot(x@lines[[j]]@Lines[[h]], "coords") <-
        cbind((sp::coordinates(x@lines[[j]]@Lines[[h]])[, 1] - x_mn) / x_mx,
              (sp::coordinates(x@lines[[j]]@Lines[[h]])[, 2] - y_mn) / y_mx)
    }
  }

  return(x)

}


# check and potentially adjust projection of Spatial* objects -------------
#' @describeIn mapControls check and potentially adjust projection of Spatial* objects
#' @export spCheckAdjustProjection
#'
#' @param verbose whether details should be printed to the console
spCheckAdjustProjection <- function(x, verbose = FALSE) {

  non_proj_waning <-
    paste("supplied", class(x)[1], "has no projection information!", "\n",
          "scaling coordinates and showing layer without background map")

  if (is.na(raster::projection(x))) {
    warning(non_proj_waning)
  } else if (!identical(raster::projection(x), llcrs)) {
    if(verbose) cat("\n", "reprojecting to web mercator", "\n\n")
    x <- sp::spTransform(x, CRSobj = llcrs)
  }

  return(x)

}


# add leaflet control button to map ---------------------------------------
#' @describeIn mapControls add leaflet control button to map
#' @export mapViewLayersControl
#'
#' @param names names of the layer groups to be added to control button
mapViewLayersControl <- function(map, map.types, names) {

  m <- leaflet::addLayersControl(map = map,
                                 position = "bottomleft",
                                 baseGroups = map.types,
                                 overlayGroups = c(
                                   getLayerNamesFromMap(map),
                                   names))
  return(m)

}


# create layer name for grouping in map -----------------------------------
#' @describeIn mapControls create layer name for grouping in map
#' @export layerName
#'
layerName <- function() {
  mvclss <- c("SpatialPointsDataFrame",
              "SpatialPolygonsDataFrame",
              "SpatialLinesDataFrame",
              "SpatialPoints",
              "SpatialPolygons",
              "SpatialLines",
              "RasterLayer",
              "RasterStack",
              "RasterBrick",
              "mapview",
              "leaflet")
  nam <- as.character(sys.calls()[[1]])
  clss <- sapply(nam, function(i) {
    try(class(dynGet(i, inherits = TRUE, minframe = 2L,
                     ifnotfound = NULL)), silent = TRUE)
  })
  indx <- which(clss %in% mvclss)
  grp <- nam[indx]
  grp <- grp[length(grp)]
  return(grp)
}


# set or calculate circle radius ------------------------------------------
#' @describeIn mapControls set or calculate circle radius
#' @export circleRadius
#'
#' @param radius either an integer specifying circle radius or the name
#' (character) of one of the columns in the SpatialPointsDataFrame
#' (attribute table)
#'
circleRadius <- function(x, radius) {
  if (is.character(radius)) {
    rad <- scale(as.numeric(x@data[, radius]), center = FALSE) * 10
  } else rad <- radius

  return(rad)
}






# extractObjectName <- function(x) {
#   pipe_splt <- strsplit(x, "%>%")[[1]][-1]
#
#   grp <- vector("character", length(pipe_splt))
#   for (i in seq(grp)) {
#     x <- pipe_splt[i]
#     tmp <- strsplit(strsplit(x,
#                              "\\(")[[1]][2], ",")[[1]][1]
#     grp[i] <- gsub("\\)", "", tmp)
#   }
#   return(grp)
# }
