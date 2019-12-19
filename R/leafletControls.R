# Convenience functions for working with spatial objects and leaflet maps
getCallMethods = function(map) {
  sapply(map$x$calls, "[[", "method")
}


getLayerControlEntriesFromMap <- function(map) {
  grep("addLayersControl", getCallMethods(map), fixed = TRUE, useBytes = TRUE)
}


getCallEntryFromMap <- function(map, call) {
  grep(call, getCallMethods(map), fixed = TRUE, useBytes = TRUE)
}



# Query leaflet map for position of 'addProviderTiles' entry --------------

getProviderTileEntriesFromMap <- function(map) {

#   seq_along(map$x$calls)[sapply(map$x$calls,
#                                 FUN = function(X) "addProviderTiles" %in% X)]
  # tst <- which(sapply(map$x$calls, function(i) {
  #   i$method == "addProviderTiles"
  # }))
  grep("addProviderTiles", getCallMethods(map), fixed = TRUE, useBytes = TRUE)
  # return(tst)

}


# Get provider tile names of leaflet map ----------------------------------

getProviderTileNamesFromMap <- function(map) {

  len <- getProviderTileEntriesFromMap(map)
  len <- len[length(len)]
  if (length(len) != 0) map$x$calls[[len]]$args[[1]] else NULL

}

# Update layer names of leaflet map ---------------------------------------

# updateLayerControlNames <- function(map1, map2) {
#   len <- getLayerControlEntriesFromMap(map1)
#   len <- len[length(len)]
#   map1$x$calls[[len]]$args[[2]] <- c(getLayerNamesFromMap(map1),
#                                      getLayerNamesFromMap(map2))
#   return(map1)
# }

# Identify layers to be hidden from initial map rendering -----------------

layers2bHidden <- function(map, hide = FALSE, ...) {

  nms <- getLayerNamesFromMap(map)

  if (hide) nms[-c(1)] else NULL

}



# Get calls from a map ----------------------------------------------------

getMapCalls <- function(map) {
  map$x$calls
}



# Get layer names of leaflet map ------------------------------------------

getLayerNamesFromMap <- function(map) {

  len <- getLayerControlEntriesFromMap(map)
  len <- len[length(len)]
  if (length(len) != 0) map$x$calls[[len]]$args[[2]] else NULL

}



# Append calls to a map ---------------------------------------------------

appendMapCallEntries_lf <- function(map1, map2) {
  ## calls
  m1_calls = map1$x$calls
  m2_calls = map2$x$calls

  ## dependencies
  m1_deps = map1$dependencies
  m2_deps = map2$dependencies

  mp_deps = append(m1_deps, m2_deps)
  mp_deps = mp_deps[!duplicated(mp_deps)]

  ## base map controls
  ctrls1 <- getLayerControlEntriesFromMap(map1)
  ctrls2 <- getLayerControlEntriesFromMap(map2)
  bmaps1 <- m1_calls[[ctrls1[1]]]$args[[1]]
  bmaps2 <- m2_calls[[ctrls2[1]]]$args[[1]]
  bmaps <- c(bmaps1, bmaps2)[!duplicated(c(bmaps1, bmaps2))]

  ## layer controls
  len1 <- ctrls1[length(ctrls1)]
  lyrs1 = if (length(len1) != 0) m1_calls[[len1]]$args[[2]] else NULL
  len2 <- ctrls2[length(ctrls2)]
  lyrs2 = if (length(len2) != 0) m2_calls[[len2]]$args[[2]] else NULL
  # lyrs1 <- getLayerNamesFromMap(map1)
  # lyrs2 <- getLayerNamesFromMap(map2)
  lyrs <- c(lyrs1, lyrs2)
  # dup <- duplicated(lyrs)
  # lyrs[dup] <- sapply(seq(lyrs[dup]), function(i) paste0(lyrs[dup][[i]], ".", as.character(i + 1)))

  ## merge
  mpcalls <- append(m1_calls, m2_calls)
  mpcalls <- mpcalls[!duplicated(mpcalls)]
  mpcalls[[ctrls1[1]]]$args[[1]] <- bmaps
  mpcalls[[ctrls1[1]]]$args[[2]] <- lyrs

  # ind <- which(sapply(mpcalls, function(i) {
  #   i$method == "addLayersControl"
  # }))

  ind =  grep("addLayersControl", sapply(mpcalls, "[[", "method"), fixed = TRUE, useBytes = TRUE)

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
  map1$dependencies = mp_deps
  return(map1)
}


appendMapCallEntries_md = function(map1, map2) {

  m1_calls = map1$x$calls
  m2_calls = map2$x$calls
  mpcalls = append(m1_calls, m2_calls)

  m1_deps = map1$dependencies
  m2_deps = map2$dependencies
  mp_deps = append(m1_deps, m2_deps)

  map1$x$calls = mpcalls
  map1$dependencies = mp_deps

  map1 = removeDuplicatedMapDependencies(map1)
  return(map1)
}

# Remove duuplicated map calls --------------------------------------------

removeDuplicatedMapCalls <- function(map) {
  ind <- duplicated(getCallMethods(map))
  if (any(ind)) map$x$calls[ind] <- NULL
  return(map)
}

# Remove duuplicated map dependencies -------------------------------------

removeDuplicatedMapDependencies <- function(map) {
  ind <- duplicated(map$dependencies)
  if (any(ind)) map$dependencies[ind] <- NULL
  return(map)
}



rasterCheckSize <- function(x, maxpixels) {
  if (maxpixels < raster::ncell(x)) {
    warning(paste("maximum number of pixels for Raster* viewing is",
                  maxpixels, "; \nthe supplied Raster* has", ncell(x), "\n",
                  "... decreasing Raster* resolution to", maxpixels, "pixels\n",
                  "to view full resolution set 'maxpixels = ", ncell(x), "'"))
    x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE, useGDAL = TRUE)
  }
  return(x)
}





# Initialise mapView base maps --------------------------------------------

initBaseMaps <- function(map.types, canvas = FALSE, viewer.suppress = FALSE) {
  ## create base map using specified map types
  if (missing(map.types)) map.types <- mapviewGetOption("basemaps")
  leafletHeight <- mapviewGetOption("leafletHeight")
  leafletWidth <- mapviewGetOption("leafletWidth")
  lid <- 1:length(map.types)
  m <- leaflet::leaflet(
    height = leafletHeight,
    width = leafletWidth,
    options = leaflet::leafletOptions(
      minZoom = 1,
      maxZoom = 52,
      bounceAtZoomLimits = FALSE,
      maxBounds = list(
        list(c(-90, -370)),
        list(c(90, 370))),
      preferCanvas = canvas),
    sizingPolicy = leafletSizingPolicy(
      viewer.suppress = viewer.suppress,
      browser.external = viewer.suppress
    )
  )
  m <- leaflet::addProviderTiles(m, provider = map.types[1],
                                 layerId = lid[1], group = map.types[1])
  if (length(map.types) > 1) {
    for (i in 2:length(map.types)) {
      m <- leaflet::addProviderTiles(m, provider = map.types[i],
                                     layerId = lid[i], group = map.types[i])
    }
  }
  return(m)
}


# Initialise mapView map --------------------------------------------------

initMap <- function(map = NULL,
                    map.types = NULL,
                    proj4str,
                    native.crs = FALSE,
                    canvas = FALSE,
                    viewer.suppress = FALSE,
                    platform = mapviewGetOption("platform"),
                    ...) {

  # if (missing(map.types)) map.types <- mapviewGetOption("basemaps")
  ls = list(...)
  nms = names(ls)

  if (platform == "leaflet") {

    if (is.null(map) & is.null(map.types)) {
      map.types <- mapviewGetOption("basemaps")
    }

    leafletHeight <- mapviewGetOption("leafletHeight")
    leafletWidth <- mapviewGetOption("leafletWidth")

    if (missing(proj4str)) proj4str <- NA
    ## create base map using specified map types
    if (is.null(map)) {
      if (is.na(proj4str) | native.crs) {
        m <- leaflet::leaflet(
          height = leafletHeight,
          width = leafletWidth,
          options = leaflet::leafletOptions(
            minZoom = -1000,
            crs = leafletCRS(crsClass = "L.CRS.Simple"),
            preferCanvas = canvas),
          sizingPolicy = leafletSizingPolicy(
            viewer.suppress = viewer.suppress,
            browser.external = viewer.suppress
          )
        )
      } else {
        m <- initBaseMaps(map.types, canvas = canvas, viewer.suppress = viewer.suppress)
      }
    } else {
      m <- map
    }

  } else if (platform == "mapdeck") {

    if (is.null(map)) {
      if (is.null(map.types)) {
        map.types <- mapviewGetOption("basemaps")
      }

      md_args = try(
        match.arg(
          nms,
          names(as.list(args(mapdeck::mapdeck))),
          several.ok = TRUE
        )
        , silent = TRUE
      )
      if (!inherits(md_args, "try-error")) {
        md_args = ls[md_args]
        md_args$style = map.types
        m = do.call(mapdeck::mapdeck, Filter(Negate(is.null), md_args))
      } else {
        m = mapdeck::mapdeck()
      }
    } else {
      m = map
    }
  }

  return(m)
}


# Scale coordinates for unprojected spatial objects -----------------------

scaleCoordinates <- function(x.coords, y.coords) {

  if (length(x.coords) == 1) {
    x_sc <- y_sc <- 0
  } else {
    ratio <- diff(range(y.coords)) / diff(range(x.coords))
    x_sc <- scales::rescale(x.coords, to = c(0, 1))
    y_sc <- scales::rescale(y.coords, to = c(0, 1)) * ratio
  }
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

  #do.call("rbind",
  pols <- lapply(seq(coord_lst), function(j) {

    ## extract current 'Polygons'
    pys <- x@polygons[[j]]

    lst <- lapply(seq(pys@Polygons), function(h) {

      # extract current 'Polygon'
      py <- pys@Polygons[[h]]

      # rescale coordinates
      crd <- sp::coordinates(py)
      coords_rscl <- cbind((crd[, 1] - x_mn) / x_mx,
                           (crd[, 2] - y_mn) / y_mx * ratio)

      # assign new coordinates and label point
      methods::slot(py, "coords") <- coords_rscl
      methods::slot(py, "labpt") <- range(coords_rscl)

      return(py)
    })

    sp::Polygons(lst, ID = pys@ID)
    # sp::SpatialPolygons(list(sp::Polygons(lst, ID = pys@ID)),
    #                     proj4string = sp::CRS(sp::proj4string(x)))
  })#)

  x@polygons <- pols

  x_rng <- range(sapply(pols, function(i) bbox(i)[1, ]))
  y_rng <- range(sapply(pols, function(i) bbox(i)[2, ]))
  x@bbox <- matrix(c(x_rng[1], x_rng[2], y_rng[1], y_rng[2]),
                   ncol = 2, byrow = TRUE)
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

  #do.call("rbind",
  lins <- lapply(seq(coord_lst), function(j) {

    ## extract current 'Lines'
    lns <- x@lines[[j]]

    lst <- lapply(seq(lns@Lines), function(h) {

      # extract current 'Line'
      ln <- lns@Lines[[h]]

      # rescale coordinates
      crd <- sp::coordinates(ln)
      coords_rscl <- cbind((crd[, 1] - x_mn) / x_mx,
                           (crd[, 2] - y_mn) / y_mx * ratio)

      # assign new coordinates and label point
      methods::slot(ln, "coords") <- coords_rscl

      return(ln)
    })

    sp::Lines(lst, ID = lns@ID)

    # sp::SpatialLines(list(sp::Lines(lst, ID = lns@ID)),
    #                     proj4string = sp::CRS(sp::proj4string(x)))
  })#)

  x@lines <- lins

  x_rng <- range(sapply(lins, function(i) bbox(i)[1, ]))
  y_rng <- range(sapply(lins, function(i) bbox(i)[2, ]))
  x@bbox <- matrix(c(x_rng[1], x_rng[2], y_rng[1], y_rng[2]),
                   ncol = 2, byrow = TRUE)
  return(x)
}



# Add leaflet control button to map ---------------------------------------

mapViewLayersControl <- function(map, map.types, names, native.crs = FALSE) {

  ind = getCallEntryFromMap(map, call = "addLayersControl")

  if (!length(ind)) {
    bgm <- map.types
  } else {
    bgm <- map$x$calls[[ind[1]]]$args[[1]]
  }

  if (!native.crs) {
    m <- leaflet::addLayersControl(map = map,
                                   position = mapviewGetOption(
                                     "layers.control.pos"),
                                   baseGroups = bgm,
                                   overlayGroups = c(
                                     getLayerNamesFromMap(map),
                                     names))
  } else {
    m <- leaflet::addLayersControl(map = map,
                                   position = mapviewGetOption(
                                     "layers.control.pos"),
                                   overlayGroups = c(
                                     getLayerNamesFromMap(map),
                                     names))
  }
  return(m)

}


# Update leaflet layers control button ------------------------------------
updateOverlayGroups = function(map, group) {
  if (inherits(map, "mapview")) map = mapview2leaflet(map)
  ind = getLayerControlEntriesFromMap(map)
  if (length(ind > 0)) {
    map$x$calls[ind][[1]]$args[[2]] =
      c(map$x$calls[ind][[1]]$args[[2]], group)
  }
  return(map)
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

# circleRadius <- function(x, radius = 8, min.rad = 3, max.rad = 20) {
#
#   if (is.character(radius)) {
#     rad <- scales::rescale(as.numeric(x@data[, radius]),
#                            to = c(min.rad, max.rad))
#   } else rad <- radius
#   return(rad)
# }



# Check sp objects --------------------------------------------------------

spCheckObject <- function(x) {

  ## convert chracter columns to factor columns
  for (i in names(x)) {
    if (is.character(x@data[, i])) {
      x@data[, i] <- as.factor(x@data[, i])
    }
  }

  ## check and remove data columns where all NA; if all columns solely contain
  ## NA values, the data columns are not omitted
  if (any(methods::slotNames(x) %in% "data")) {
    all_na_index <- sapply(seq(x@data), function(i) {
      all(is.na(x@data[, i]))
    })
    if (any(all_na_index)) {
      if (all(all_na_index)) {
        cl <- gsub("DataFrame", "", class(x)[1])
        warning("Attribute table associated with 'x' contains only NA values. Converting to '", cl, "' object.")
        x <- as(x, cl)
      } else {
        warning("Columns ",
                paste(colnames(x@data)[all_na_index], collapse = ", "),
                " in attribute table contain only NA values and are dropped.")
        x <- x[, !all_na_index]
      }
    }
  }

  return(x)
}

### print.saveas --------------------------------------------------------

#print.saveas <- function(x, ...){
#  class(x) = class(x)[class(x)!="saveas"]
#  htmltools::save_html(x, file=attr(x,"filesave"))
#}

### print.saveas --------------------------------------------------------

#saveas <- function(map, file){
#  class(map) <- c("saveas",class(map))
#  attr(map,"filesave")=file
#  map
#}




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
