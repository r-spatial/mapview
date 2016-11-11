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
#' garnishMap(m, addPolylines, data = st_as_sfc(atlStorms2005))
#' garnishMap(m, addPolygons, data = st_as_sfc(gadmCHE))
#' garnishMap(m, addCircleMarkers, data = st_as_sfc(breweries91))
#'
#' }
#'
#' @export polygonData.sfc
#' @name polygonData.sfc
#' @rdname polygonData.sfc
#' @method polygonData sfc
#' @aliases polygonData.sfc
#' @importFrom sf st_geometry
"polygonData.sfc" <- function(obj) {

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

  bbx <- matrix(bb, ncol = 2, byrow = TRUE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))

  attributes(tmp) <- list(bbox = bbx)

  return(tmp)

}

#' @export pointData.sfc
#' @describeIn polygonData.sfc method for point data
#' @method pointData sfc
#' @aliases pointData.sfc
# pointData.sfc <- function(obj) {
#
#   tmp <- do.call("rbind", lapply(geometry(obj), function(i) {
#     lng = i[[1]]
#     lat = i[[2]]
#     data.frame(lng = lng, lat = lat)
#   }))
#
# }

"pointData.sfc" <- function(obj) {
  tmp <- do.call("rbind", lapply(st_geometry(obj), function(i) {
    lng = i[[1]]
    lat = i[[2]]
    data.frame(lng = lng, lat = lat)
  }))
  return(tmp)
}

#' @export pointData.MULTIPOINT
#' @describeIn polygonData.sfc method for point data
#' @method pointData MULTIPOINT
#' @method pointData MULTIPOINT Z
#' @method pointData MULTIPOINT M
#' @method pointData MULTIPOINT ZM
#' @aliases pointData.MULTIPOINT,pointData.MULTIPOINT Z,pointData.MULTIPOINT M,pointData.MULTIPOINT ZM
"pointData.MULTIPOINT" <-
  "pointData.MULTIPOINT Z" <-
  "pointData.MULTIPOINT M" <-
  "pointData.MULTIPOINT ZM" <- function(obj) {
    lng <- obj[, 1]
    lat <- obj[, 2]
    data.frame(lng = lng, lat = lat)
  }

## This is NOT RIGHT yet MDS
##
#' @export pointData.sfc_POINT
#' @describeIn polygonData.sfc method for point data
#' @method pointData sfc_POINT
#' @method pointData sfc_POINT Z
#' @method pointData sfc_POINT M
#' @method pointData sfc_POINT ZM
#' @aliases "pointData.sfc_POINT","pointData.POINT Z","pointData.POINT M","pointData.POINT ZM"
"pointData.sfc_POINT" <-
  "pointData.sfc_POINT Z" <-
  "pointData.sfc_POINT M" <-
  "pointData.sfc_POINT ZM" <- function(obj) {
    data.frame(lng = obj[[1]][[1]], lat = obj[[1]][[2]])
  }
