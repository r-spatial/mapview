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
#' garnishMap(m, addPolylines, data = as.sf(atlStorms2005))
#' garnishMap(m, addPolygons, data = as.sf(gadmCHE))
#' garnishMap(m, addCircleMarkers, data = as.sf(breweries91))
#'
#' }
#'
#' @export polygonData.sf
#' @name polygonData.sf
#' @rdname polygonData.sf
#' @aliases polygonData.sf


"polygonData.sf" <- function(obj) {

  if (inherits(geometry(obj)[[1]], "MULTIPOLYGON")) {
    tmp <- lapply(geometry(obj), function(i) {
      lapply(i, function(j) {
        lng <- j[[1]][, 1]
        lat <- j[[1]][, 2]
        list(lng = lng, lat = lat)
      })
    })
  } else if (inherits(geometry(obj)[[1]], "LINESTRING")) {
    tmp <- lapply(geometry(obj), function(i) {
      #lapply(i, function(j) {
      lng <- i[, 1]
      lat <- i[, 2]
      list(list(lng = lng, lat = lat))
      #})
    })
  }

  bb <- attributes(geometry(obj))$bbox

  bbx <- matrix(bb, ncol = 2, byrow = TRUE)
  attr(bbx, "dimnames") <- list(c("x", "y"),
                                c("min", "max"))

  attributes(tmp) <- list(bbox = bbx)

  return(tmp)

}

#' @export pointData.sf
#' @describeIn polygonData.sf method for point data
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
  tmp <- do.call("rbind", lapply(geometry(obj), function(i) {
    lng = i[[1]]
    lat = i[[2]]
    data.frame(lng = lng, lat = lat)
  }))
  return(tmp)
}

#' @export pointData.MULTIPOINT
#' @describeIn polygonData.sf method for point data
#' @aliases pointData.MULTIPOINT,pointData.MULTIPOINT Z,pointData.MULTIPOINT M,pointData.MULTIPOINT ZM
"pointData.MULTIPOINT" <-
  "pointData.MULTIPOINT Z" <-
  "pointData.MULTIPOINT M" <-
  "pointData.MULTIPOINT ZM" <- function(obj) {
    lng <- obj[, 1]
    lat <- obj[, 2]
    data.frame(lng = lng, lat = lat)
  }

#' @export pointData.POINT
#' @describeIn polygonData.sf method for point data
#' @aliases pointData.POINT,"pointData.POINT Z","pointData.POINT M","pointData.POINT ZM"
"pointData.POINT" <-
  "pointData.POINT Z" <-
  "pointData.POINT M" <-
  "pointData.POINT ZM" <- function(obj) {
    data.frame(lng = obj[1], lat = obj[2])
  }
