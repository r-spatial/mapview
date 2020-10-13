
isAvailableInLeaflet = function() {
  return(
    list(
      lab = "label" %in% names(as.list(args(leaflet::addCircleMarkers))),
      scl = "addScaleBar" %in% ls(getNamespace("leaflet")),
      leggrp = "group" %in% names(as.list(args(leaflet::addLegend)))
    )
  )
}

# lab_avl = isAvailableInLeaflet()$lab
# scl_avl = isAvailableInLeaflet()$scl

warn = paste("Feature labels on mouseover and 'addScaleBar' are not supported in the installed version of 'leaflet'.",
             "\nRun devtools::install_github('rstudio/leaflet') and re-install 'mapview' locally to enable these features.")




### mapview to leaflet
mapview2leaflet = function(x) {
  methods::slot(x, "map")
}


### mapview simple class
getSimpleClass = function(obj) {
  if (inherits(obj, c("RasterLayer", "RasterStack",
                      "RasterBrick", "Satellite",
                      "SpatialGridDataFrame",
                      "SpatialPixelsDataFrame"))) "rst" else "vec"
}


### labels
makeLabels = function(x, zcol = NULL) {
  if (inherits(x, "XY")) {
    lab = "1"
  } else if (inherits(x, "sfc")) {
    lab = as.character(seq(length(x)))
  } else if (inherits(x, "sf") & is.null(zcol)) {
    lab = rownames(x)
  } else if (inherits(x, "Raster")) {
    lab = TRUE
  } else lab = as.character(as.data.frame(x)[, zcol])
  return(lab)
}


makeLabelsSP = function(col) {
  as.character(col)
}


### getFeatureIds
getFeatureIds = function(att_data) {
  if (inherits(att_data, "Spatial") | inherits(att_data, "sf")) {
    ids = row.names(att_data)
  } else if (inherits(att_data, "sfc")) {
    ids = seq(length(att_data))
  }

  return(ids)
}


### createExtent
createExtent = function(x, offset = NULL) {

  if (inherits(x, "Extent")) {
    return(x)
  } else {
    if (inherits(x, "Raster")) {
      ext = raster::extent(
        raster::projectExtent(x, crs = sp::CRS("+init=epsg:4326")))
    } else if (inherits(x, "Spatial")) {
      ext = raster::extent(raster::xmin(x),
                           raster::xmax(x),
                           raster::ymin(x),
                           raster::ymax(x))
    } else if (inherits(x, "sfc") | inherits(x, "sf") |
               inherits(x, "XY") | inherits(x, "stars")) {
      bb = sf::st_bbox(x)
      ext = raster::extent(bb[1], bb[3], bb[2], bb[4])
    }

    if (is.null(offset)) {
      xxtend = c(ext[1], ext[2])
      yxtend = c(ext[3], ext[4])
      ext@xmin = xxtend[1]
      ext@xmax = xxtend[2]
      ext@ymin = yxtend[1]
      ext@ymax = yxtend[2]
    } else {
      ext@xmin = ext@xmin - offset
      ext@xmax = ext@xmax + offset
      ext@ymin = ext@ymin - offset
      ext@ymax = ext@ymax + offset
    }

    return(ext)
  }

}


isMultiFeature = function(x) {
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
getSFClass = function(x) {
  if (class(x)[1] == "XY") class(x)[2] else class(x)[1]
}


getGeometryType = function(x) {
  # raster / stars
  if (inherits(x, c("Raster", "stars"))) {
    return("rs")
  }

  # sf
  if (inherits(x, "Spatial")) x = sf::st_as_sfc(x)
  g = sf::st_geometry(x)
  if (inherits(g, "POINT") |
      inherits(g, "MULTIPOINT") |
      inherits(g, "sfc_POINT") |
      inherits(g, "sfc_MULTIPOINT")) type = "pt"
  if (inherits(g, "LINESTRING") |
      inherits(g, "MULTILINESTRING") |
      inherits(g, "sfc_LINESTRING") |
      inherits(g, "sfc_MULTILINESTRING")) type = "ln"
  if (inherits(g, "POLYGON") |
      inherits(g, "MULTIPOLYGON") |
      inherits(g, "sfc_POLYGON") |
      inherits(g, "sfc_MULTIPOLYGON")) type = "pl"
  if (inherits(g, "sfc_GEOMETRY") |
      inherits(g, "sfc_GEOMETRYCOLLECTION")) type = "gc" #getGeometryType(sf::st_cast(g))
  return(type)
}


getMaxFeatures = function(x) {
  switch(getGeometryType(x),
         "pt" = 40000,
         "ln" = 300000,
         "pl" = 300000,
         "gc" = 300000)
}


lineWidth = function(x) {
  lw = switch(getGeometryType(x),
              "pt" = 1,
              "ln" = 2,
              "pl" = 0.5,
              "gc" = 2)
  return(lw)
}


regionOpacity = function(x) {
  switch(
    getGeometryType(x)
    , "pt" = ifelse(mapviewGetOption("platform") == "leafgl", 0.8, 0.6)
    , "ln" = 1
    , "pl" = ifelse(mapviewGetOption("platform") == "leafgl", 0.8, 0.6)
    , "gc" = ifelse(mapviewGetOption("platform") == "leafgl", 0.8, 0.6)
    , "rs" = 0.8
  )
}


basemaps = function(colors) {
  ml = mean(as.numeric(sapply(colors, luminence)))
  if (length(unique(colors)) == 1) {
    unique_cyan = ifelse(unique(colors) %in% c("cyan", "#00ffff", "#00FFFF"), TRUE, FALSE)
  } else {
    unique_cyan = FALSE
  }
  if (ml > 0.8 | unique_cyan) {
    mapviewGetOption("basemaps")[c(2, 1, 3:5)]
  } else {
    mapviewGetOption("basemaps")
  }
}


getProjection = function(x) {

  if (inherits(x, c("Raster", "Spatial"))) {
    prj = raster::projection(x)
  } else {
    prj = sf::st_crs(x)$proj4string
  }

  return(prj)

}


createFileId = function(ndigits = 6) {
  paste(sample(c(letters[1:6], 0:9), ndigits), collapse = "")
}


extendLimits = function(lim, length = 1, prop = 0.07) {
  if (length(lim) != 2) stop("lim should be of length 2")
  if (lim[1] > lim[2]) lim = rev(lim)
  if (!missing(length)) {
    prop = (as.numeric(length) -
              as.numeric(diff(lim))) / (2 * as.numeric(diff(lim)))
  }
  if (isTRUE(all.equal(lim[1], lim[2]))) {
    lim + 0.005 * c(-length, length)
  } else {
    d = diff(as.numeric(lim))
    lim + prop * d * c(-1, 1)
  }
}


circleRadius = function(x, radius = 6, min.rad = 3, max.rad = 15, na.rad = 2, ...) {

  if (is.character(radius)) {
    rad = scales::rescale(as.numeric(x[[radius]]),
                          to = c(min.rad, max.rad))
    rad[is.na(rad)] = na.rad
  } else rad = radius
  return(rad)
}


extentOverlap = function(x, y) {
  if (!sum(lengths(sf::st_intersects(x, y))) == 0) TRUE else FALSE
}


makeLayerName = function(x, zcol = NULL, up = 3) {
  lnm = deparse(substitute(x, env = parent.frame(up)), width.cutoff = 500)
  lnm = toString(lnm[1], width = 50)
  if (is.null(zcol)) lnm else paste(lnm, zcol, sep = " - ")
}



makeListLayerNames = function(x, layer.name) {
  if (length(layer.name) == length(x) & !(is.list(x))) {
    lnms = layer.name
  } else if (is.list(x) & !(is.null(names(x)))) {
    lnms = names(x)
  } else {
    chr = gsub(utils::glob2rx("*list(*"), "", layer.name)
    chr = unlist(strsplit(x = gsub(")", "", chr), ","))
    if (length(chr) / length(x) == 2) {
      idx = seq(1, length(chr), 2)
      lnms = paste(chr[idx], chr[idx + 1], sep = ",")
    } else {
      lnms = gsub(" ", "", chr)
    }
  }

  if (length(lnms) == 1 & length(x) > 1) {
    lnms = lapply(seq(x), function(i) {
      paste0(lnms, "[[", i , "]]")
    })
  }

  return(as.list(lnms))
}


paneName = function(x) {
  if (inherits(x, "stars")) {
    return("stars")
  }
  switch(getGeometryType(x),
         "pt" = "point",
         "ln" = "line",
         "pl" = "polygon",
         "gc" = "gcollection")
}

zIndex = function(x) {
  if (inherits(x, "stars")) {
    return(400)
  }
  switch(getGeometryType(x),
         "pt" = 440,
         "ln" = 430,
         "pl" = 420,
         "gc" = 410)
}


useCanvas = function(x) {
  if (inherits(x, "list")) {
    lst = sapply(x, useCanvas)
    ifelse(any(lst), TRUE, FALSE)
  } else {
    switch(
      getGeometryType(x),
      "pt" = ifelse(featureComplexity(x) > 500, TRUE, FALSE),
      "ln" = ifelse(featureComplexity(x) > 5000, TRUE, FALSE),
      "pl" = ifelse(featureComplexity(x) > 2000, TRUE, FALSE),
      "gc" = ifelse(featureComplexity(x) > 500, TRUE, FALSE)
    )
  }
}

is_literally_false = function(x) {
  if (getRversion() >= 3.5) {
    isFALSE(x)
  } else {
    is.logical(x) && length(x) == 1L && !is.na(x) && !x
  }
}

listifyer = function(x, by_row = FALSE) {
  if (by_row) {
    strct = sapply(x, function(i) {
      if (inherits(i, "sfc")) {
        length(i)
      }
      if (inherits(i, "sf")) {
        nrow(i)
      }
    })
    idx = rep(1:length(x), times = strct)
    return(
      function(arg) {
        arg_nm = deparse(substitute(arg))
        arg = unlist(arg)
        if (length(arg) == 1) {
          return(rep(arg, length(idx)))
        }
        if (length(arg) > 1 && length(arg) <= length(idx)) {
          splt = split(arg, idx)
          if (arg_nm == "popup") {
            splt = sapply(splt, function(i) {
              attr(i, "popup") = "leafpop"
              return(i)
            })
          }
          return(splt)
        }
      }
    )
  }

  idx = length(x)
  function(arg, as_list = FALSE) {
    arg_nm = deparse(substitute(arg))
    if (inherits(x[[1]], c("Raster", "stars")) &&
        arg_nm %in% c("popup")) {
      return(NULL)
    }
    if (as_list) {
      return(as.list(arg))
    }
    if (is.function(arg)) {
      return(replicate(idx, arg))
    }
    if (is.list(arg) && length(arg) == idx) {
      return(arg)
    }
    if (!is.list(arg) && length(arg) == idx) {
      return(as.list(arg))
    }
    return(rep(list(arg), idx))
  }

}
