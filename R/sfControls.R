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
# pointData.sf <- function(obj) {
#
#   tmp <- do.call("rbind", lapply(geometry(obj), function(i) {
#     lng = i[[1]]
#     lat = i[[2]]
#     data.frame(lng = lng, lat = lat)
#   }))
#
# }

"pointData.sf" <- function(obj) {
  tmp <- do.call("rbind", lapply(st_geometry(obj), function(i) {
    lng = i[[1]]
    lat = i[[2]]
    data.frame(lng = lng, lat = lat)
  }))
  return(tmp)
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

## This is NOT RIGHT yet MDS
##
#' @export pointData.POINT
#' @describeIn polygonData.sf method for point data
#' @method pointData POINT
#' @aliases "pointData.POINT"
"pointData.POINT" <- function(obj) {
    data.frame(lng = obj[[1]], lat = obj[[2]])
  }



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


