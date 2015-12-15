# Convenience functions for working with spatial objects and leaflet maps

getLayerControlEntriesFromMap <- function(map) {

#   seq_along(map$x$calls)[sapply(map$x$calls,
#                                 FUN = function(X) "addLayersControl" %in% X)]
  tst <- which(sapply(map$x$calls, function(i) {
    i$method == "addLayersControl"
  }))
  return(tst)

}


getCallEntryFromMap <- function(map, call) {

  #   seq_along(map$x$calls)[sapply(map$x$calls,
  #                                 FUN = function(X) "addLayersControl" %in% X)]
  tst <- which(sapply(map$x$calls, function(i) {
    i$method == call
  }))
  return(tst)

}


# Get layer names of leaflet map ------------------------------------------

getLayerNamesFromMap <- function(map) {

  len <- getLayerControlEntriesFromMap(map)
  len <- len[length(len)]
  if (length(len) != 0) map$x$calls[[len]]$args[[2]] else NULL

}


# Query leaflet map for position of 'addProviderTiles' entry --------------

getProviderTileEntriesFromMap <- function(map) {

#   seq_along(map$x$calls)[sapply(map$x$calls,
#                                 FUN = function(X) "addProviderTiles" %in% X)]
  tst <- which(sapply(map$x$calls, function(i) {
    i$method == "addProviderTiles"
  }))
  return(tst)

}


# Get provider tile names of leaflet map ----------------------------------

getProviderTileNamesFromMap <- function(map) {

  len <- getProviderTileEntriesFromMap(map)
  len <- len[length(len)]
  if (length(len) != 0) map$x$calls[[len]]$args[[1]] else NULL

}

# Update layer names of leaflet map ---------------------------------------

updateLayerControlNames <- function(map1, map2) {
  len <- getLayerControlEntriesFromMap(map1)
  len <- len[length(len)]
  map1$x$calls[[len]]$args[[2]] <- c(getLayerNamesFromMap(map1),
                                     getLayerNamesFromMap(map2))
  return(map1)
}

# Identify layers to be hidden from initial map rendering -----------------

layers2bHidden <- function(map) {

  nms <- getLayerNamesFromMap(map)
  nms[-c(1)]

}



# Get calls from a map ----------------------------------------------------

getMapCalls <- function(map) {
  map$x$calls
}



# Append calls to a map ---------------------------------------------------

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

  ind <- which(sapply(mpcalls, function(i) {
    i$method == "addLayersControl"
  }))

#   ind <- seq_along(mpcalls)[sapply(mpcalls,
#                                    FUN = function(X) {
#                                      "addLayersControl" %in% X
#                                      })]
  ind1 <- ind[1]
  ind2 <- ind[-1]
  try({
    mpcalls[[ind2]] <- mpcalls[[ind1]]
    mpcalls[[ind1]] <- NULL
  }, silent = TRUE)

  map1$x$calls <- mpcalls
  return(map1)
}



# Remove duuplicated map calls --------------------------------------------

removeDuplicatedMapCalls <- function(map) {
  ind <- anyDuplicated(map$x$calls)
  for (i in ind) map$x$calls[[ind]] <- NULL
  return(map)
}



wmcrs <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
llcrs <- "+proj=longlat +datum=WGS84 +no_defs"


# Check size of Raster* objects for mapView -------------------------------

rasterCheckSize <- function(x, maxpixels) {
  if (maxpixels < raster::ncell(x)) {
    warning(paste("maximum number of pixels for Raster* viewing is",
                  maxpixels, "the supplied Raster* has", ncell(x), "\n",
                  "... decreasing Raster* resolution to", maxpixels, "pixels\n",
                  "to view full resolution adjust 'maxpixels = ...'"))
    x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE, useGDAL = TRUE)
  }
  return(x)
}



# Project Raster* objects for mapView -------------------------------------

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


# Initialise mapView base maps --------------------------------------------

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


# Initialise mapView map --------------------------------------------------

initMap <- function(map, map.types, proj4str) {

  if (missing(map.types)) map.types <- c("OpenStreetMap",
                                         "Esri.WorldImagery")
  if (missing(map) & missing(map.types)) {
    map <- NULL
    map.types <- c("OpenStreetMap",
                   "Esri.WorldImagery")
  }
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


# Scale coordinates for unprojected spatial objects -----------------------

scaleCoordinates <- function(x.coords, y.coords) {

  ratio <- diff(range(y.coords)) / diff(range(x.coords))
  x_sc <- scales::rescale(x.coords, to = c(0, 1))
  y_sc <- scales::rescale(y.coords, to = c(0, 1)) * ratio
  return(cbind(x_sc, y_sc))

}



# Scale extent ------------------------------------------------------------

scaleExtent <- function(x) {
  ratio <- raster::nrow(x) / raster::ncol(x)
  x_sc <- scales::rescale(c(x@extent@xmin, x@extent@xmax), c(0, 1))
  y_sc <- scales::rescale(c(x@extent@ymin, x@extent@ymax), c(0, 1)) * ratio

  return(raster::extent(c(x_sc, y_sc)))
}


# Scale unprojected SpatialPolygons* objects ------------------------------

scalePolygonsCoordinates <- function(x) {

  coord_lst <- lapply(methods::slot(x, "polygons"), function(x) {
    lapply(methods::slot(x, "Polygons"), function(y) methods::slot(y, "coords"))
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
      methods::slot(x@polygons[[j]]@Polygons[[h]], "coords") <-
        cbind((sp::coordinates(x@polygons[[j]]@Polygons[[h]])[, 1] - x_mn) / x_mx,
              (sp::coordinates(x@polygons[[j]]@Polygons[[h]])[, 2] - y_mn) / y_mx)
    }
  }

  return(x)

}


# Scale unprojected SpatialLines* objects ------------------------------

scaleLinesCoordinates <- function(x) {

  coord_lst <- lapply(methods::slot(x, "lines"), function(x) {
    lapply(methods::slot(x, "Lines"), function(y) methods::slot(y, "coords"))
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
      methods::slot(x@lines[[j]]@Lines[[h]], "coords") <-
        cbind((sp::coordinates(x@lines[[j]]@Lines[[h]])[, 1] - x_mn) / x_mx,
              (sp::coordinates(x@lines[[j]]@Lines[[h]])[, 2] - y_mn) / y_mx)
    }
  }

  return(x)

}


# Check and potentially adjust projection of Spatial* objects -------------

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
  } else if (!identical(raster::projection(x), llcrs)) {
    x <- sp::spTransform(x, CRSobj = llcrs)
  }

  return(x)

}

# Check projection of objects according to their keywords -------

compareProjCode <- function (x){
  proj <- datum <- nodefs <- "FALSE"
  allWGS84<- as.vector(c("+init=epsg:4326", "+proj=longlat", "+datum=WGS84", "+no_defs", "+ellps=WGS84", "+towgs84=0,0,0"))

  for (comp in allWGS84) {

    if (comp %in% x[[1]]) {
        if (comp == "+init=epsg:4326") {
          proj <- datum <- nodefs <- "TRUE"
        }
        if (comp == "+proj=longlat") {
         proj<- "TRUE"
        }
        if (comp == "+no_defs") {
        nodefs<-"TRUE"
        }
        if (comp == "+datum=WGS84") {
        datum<-"TRUE"
        }
    }
  }
  if (proj == "TRUE" & nodefs == "TRUE" &  datum == "TRUE") {
    x<-llcrs
  } else {
    x<- paste(x[[1]], collapse = ' ')
  }
  return(x)
  }


# Check and potentially adjust projection of objects to be rendered -------

checkAdjustProjection <- function(x) {

  if (class(x)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    x <- rasterCheckAdjustProjection(x)
  } else if (class(x)[1] %in% c("SpatialPointsDataFrame",
                                "SpatialPolygonsDataFrame",
                                "SpatialLinesDataFrame",
                                "SpatialPoints",
                                "SpatialPolygons",
                                "SpatialLines")) {
    x <- spCheckAdjustProjection(x)
  }

  return(x)
}





# Add leaflet control button to map ---------------------------------------

mapViewLayersControl <- function(map, map.types, names) {

  m <- leaflet::addLayersControl(map = map,
                                 position = mapviewGetOption(
                                   "layers.control.pos"),
                                 baseGroups = map.types,
                                 overlayGroups = c(
                                   getLayerNamesFromMap(map),
                                   names))
  return(m)

}


# Create layer name for grouping in map -----------------------------------

# layerName <- function() {
#   mvclss <- c("SpatialPointsDataFrame",
#               "SpatialPolygonsDataFrame",
#               "SpatialLinesDataFrame",
#               "SpatialPoints",
#               "SpatialPolygons",
#               "SpatialLines",
#               "RasterLayer",
#               "RasterStack",
#               "RasterBrick",
#               "mapview",
#               "leaflet")
#   nam <- as.character(sys.calls()[[1]])
#   clss <- sapply(nam, function(i) {
#     try(class(dynGet(i, inherits = TRUE, minframe = 2L,
#                      ifnotfound = NULL)), silent = TRUE)
#   })
#   indx <- which(clss %in% mvclss)
#   grp <- nam[indx]
#   grp <- grp[length(grp)]
#   return(grp)
# }


# Set or calculate circle radius ------------------------------------------

circleRadius <- function(x, radius) {
  if (is.character(radius)) {
    rad <- scales::rescale(as.numeric(x@data[, radius]), to = c(3, 20))
  } else rad <- radius

  return(rad)
}



# Check sp objects --------------------------------------------------------

spCheckObject <- function(x, verbose) {

  ## check and remove data columns where all NA
  if (any(methods::slotNames(x) %in% "data")) {
    all_na_index <- sapply(seq(x@data), function(i) {
      all(is.na(x@data[, i]))
    })
    if(verbose & any(all_na_index)) {
      cat(paste("columns:",
                paste(colnames(x@data)[all_na_index],
                      collapse = "and"),
                "in attribute table only have NA values and are dropped"))
    }
    x <- x[, !all_na_index]
  }
  return(x)
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
