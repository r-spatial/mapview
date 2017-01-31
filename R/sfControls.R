#' Methods for simple features
#'
#' @description
#' These functions provide methods for simple features.
#'
#' @param obj a simple features object.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' m <- leaflet() %>% addTiles()
#'
#' garnishMap(m, addPolylines, data = st_as_sf(atlStorms2005))
#' garnishMap(m, addPolygons, data = st_as_sf(gadmCHE))
#' garnishMap(m, addCircleMarkers, data = st_as_sf(breweries91))
#'
#' }
#'
#' @export polygonData.sf
#' @name polygonData.sf
#' @rdname polygonData.sf
#' @method polygonData sf
#' @aliases polygonData.sf
#' @importFrom sf st_geometry
"polygonData.sf" <- function(obj) {

  obj_geom <- sf::st_geometry(obj)
  if (inherits(obj_geom[[1]], "MULTIPOLYGON")) {
    tmp <- lapply(obj_geom, function(i) {
      lapply(i, function(j) {
        lng <- j[[1]][, 1]
        lat <- j[[1]][, 2]
        list(lng = lng, lat = lat)
      })
    })
  } else if (inherits(obj_geom[[1]], "LINESTRING")) {
    tmp <- lapply(obj_geom, function(i) {
      #lapply(i, function(j) {
      lng <- i[, 1]
      lat <- i[, 2]
      list(list(lng = lng, lat = lat))
      #})
    })
  }

  bb <- attributes(obj_geom)$bbox

  bbx <- matrix(bb, ncol = 2, byrow = FALSE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))

  attributes(tmp) <- list(bbox = bbx)

  return(tmp)

}

#' @export pointData.sf
#' @describeIn polygonData.sf method for point data
#' @method pointData sf
#' @aliases pointData.sf
"pointData.sf" <- function(obj) {
  tmp <- do.call("rbind", lapply(sf::st_geometry(obj), function(i) {
    lng = i[[1]]
    lat = i[[2]]
    data.frame(lng = lng, lat = lat)
  }))
  return(tmp)
}

### POINT =================================================================
#' @export pointData.POINT
#' @describeIn polygonData.sf method for point data
#' @method pointData POINT
#' @aliases "pointData.POINT"
"pointData.POINT" <- function(obj) {
  data.frame(lng = obj[[1]], lat = obj[[2]])
}


#' @export pointData.MULTIPOINT
#' @describeIn polygonData.sf method for point data
#' @method pointData MULTIPOINT
#' @aliases pointData.MULTIPOINT
"pointData.MULTIPOINT" <- function(obj) {
    lng <- obj[, 1]
    lat <- obj[, 2]
    data.frame(lng = lng, lat = lat)
  }


#' @export pointData.sfc_POINT
#' @describeIn polygonData.sf method for point data
#' @method pointData sfc_POINT
#' @aliases "pointData.sfc_POINT"
"pointData.sfc_POINT" <- function(obj) {
  tmp <- do.call("rbind", lapply(obj, function(i) {
    lng = i[[1]]
    lat = i[[2]]
    data.frame(lng = lng, lat = lat)
  }))
  return(tmp)
}


#' @export pointData.sfc_MULTIPOINT
#' @describeIn polygonData.sf method for multipoint data
#' @method pointData sfc_MULTIPOINT
#' @aliases "pointData.sfc_MULTIPOINT"
"pointData.sfc_MULTIPOINT" <- function(obj) {
  tmp <- do.call("rbind", lapply(obj, function(i) {
    lng <- i[, 1]
    lat <- i[, 2]
  data.frame(lng = lng, lat = lat)
  }))
  return(tmp)
}


### POLYGON ===============================================================
#### polygons
nestPolygons <- function(obj) {
  tmp <- lapply(obj, function(i) {
    lng <- i[, 1]
    lat <- i[, 2]
    list(lng = lng, lat = lat)
  })
  tmp <- list(tmp)
  return(tmp)
}



#' @export polygonData.POLYGON
#' @describeIn polygonData.sf method for polygon data
#' @method polygonData POLYGON
#' @aliases "polygonData.POLYGON"
"polygonData.POLYGON" <- function(obj) {
  tmp <- nestPolygons(obj)

  bb <- st_bbox(obj)
  bbx <- matrix(bb, ncol = 2, byrow = FALSE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))
  attributes(tmp) <- list(bbox = bbx)

  return(tmp)
}


#' @export polygonData.MULTIPOLYGON
#' @describeIn polygonData.sf method for polygon data
#' @method polygonData MULTIPOLYGON
#' @aliases "polygonData.MULTIPOLYGON"
"polygonData.MULTIPOLYGON" <- function(obj) {
  tmp <- sapply(obj, function(i) {
    nestPolygons(i)
  })

  bb <- st_bbox(obj)
  bbx <- matrix(bb, ncol = 2, byrow = FALSE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))
  attributes(tmp) <- list(bbox = bbx)

  return(tmp)
}


#' @export polygonData.sfc_POLYGON
#' @describeIn polygonData.sf method for polygon data
#' @method polygonData sfc_POLYGON
#' @aliases "polygonData.sfc_POLYGON"
"polygonData.sfc_POLYGON" <- function(obj) {
  tmp <- sapply(obj, function(i) {
    nestPolygons(i)
  })

  bb <- st_bbox(obj)
  bbx <- matrix(bb, ncol = 2, byrow = FALSE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))
  attributes(tmp) <- list(bbox = bbx)

  return(tmp)
}


#' @export polygonData.sfc_MULTIPOLYGON
#' @describeIn polygonData.sf method for multipolygon data
#' @method polygonData sfc_MULTIPOLYGON
#' @aliases "polygonData.sfc_MULTIPOLYGON"
"polygonData.sfc_MULTIPOLYGON" <- function(obj) {
  tmp <- lapply(obj, function(i) {
    lapply(i, function(j) {
      lng <- j[[1]][, 1]
      lat <- j[[1]][, 2]
      list(lng = lng, lat = lat)
    })
  })

  bb <- st_bbox(obj)
  bbx <- matrix(bb, ncol = 2, byrow = FALSE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))
  attributes(tmp) <- list(bbox = bbx)

  return(tmp)
}

### LINE ==================================================================
### lines
nestLines <- function(obj) {
  lng <- obj[, 1]
  lat <- obj[, 2]
  tmp <- list(lng = lng, lat = lat)
  return(list(list(tmp)))
}


#' @export polygonData.LINESTRING
#' @describeIn polygonData.sf method for line data
#' @method polygonData LINESTRING
#' @aliases "polygonData.LINESTRING"
"polygonData.LINESTRING" <- function(obj) {
  tmp <- nestLines(obj)

  bb <- st_bbox(obj)
  bbx <- matrix(bb, ncol = 2, byrow = FALSE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))
  attributes(tmp) <- list(bbox = bbx)

  return(tmp)
}


#' @export polygonData.MULTILINESTRING
#' @describeIn polygonData.sf method for line data
#' @method polygonData MULTILINESTRING
#' @aliases "polygonData.MULTILINESTRING"
"polygonData.MULTILINESTRING" <- function(obj) {
  tmp <- sapply(obj, function(i) {
    nestLines(i)
  })

  bb <- st_bbox(obj)
  bbx <- matrix(bb, ncol = 2, byrow = FALSE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))
  attributes(tmp) <- list(bbox = bbx)

  return(tmp)
}


#' @export polygonData.sfc_LINESTRING
#' @describeIn polygonData.sf method for line data
#' @method polygonData sfc_LINESTRING
#' @aliases "polygonData.sfc_LINESTRING"
"polygonData.sfc_LINESTRING" <- function(obj) {
  # tmp <- sapply(obj, function(i) {
  #   nestLines(obj)
  # })
  tmp <- lapply(obj, function(i) {
    #lapply(i, function(j) {
    lng <- i[, 1]
    lat <- i[, 2]
    list(list(lng = lng, lat = lat))
    #})
  })

  bb <- st_bbox(obj)
  bbx <- matrix(bb, ncol = 2, byrow = FALSE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))
  attributes(tmp) <- list(bbox = bbx)

  return(tmp)
}


#' @export polygonData.sfc_MULTILINESTRING
#' @describeIn polygonData.sf method for line data
#' @method polygonData sfc_MULTILINESTRING
#' @aliases "polygonData.sfc_MULTILINESTRING"
"polygonData.sfc_MULTILINESTRING" <- function(obj) {
  # tmp <- sapply(obj, function(i) {
  #   nestLines(obj)
  # })
  tmp <- lapply(obj, function(j) {
    sapply(j, function(i) {
      #lapply(i, function(j) {
      lng <- i[, 1]
      lat <- i[, 2]
      list(list(lng = lng, lat = lat))
      #})
    })
  })

  bb <- st_bbox(obj)
  bbx <- matrix(bb, ncol = 2, byrow = FALSE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))
  attributes(tmp) <- list(bbox = bbx)

  return(tmp)
}

### MISC ==================================================================
sf2DataFrame <- function(x) {
  stopifnot(inherits(x, "sf") | inherits(x, "sfc"))
  if (inherits(x, "sf")) {
    geompos <- which(names(x) == "geometry")
    return(data.frame(x)[, -geompos, drop = FALSE])
  } else {
    d <- data.frame("a" = seq(length(x)))
    names(d) <- "Feature ID"
    return(d)
  }
}


nNodes = function(x) {
  sum(sapply(x, function(y) {
    if (is.list(y)) nNodes(y) else nrow(y)
  }))
}


npts = function(x) {
  if (getGeometryType(x) == "pt") {
    length(sf::st_geometry(x))
  } else {
    nNodes(sf::st_geometry(x))
  }
}

#100000 / (npts(st_geometry(st_as_sf(gadmCHE))) / length(st_geometry(st_as_sf(gadmCHE))))
