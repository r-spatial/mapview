#' Convert points to SpatialPolygons*
#'
#' @description
#' Create a \code{SpatialPolygons*} object from a set of point coordinates in
#' one go, i.e. without being required to run through the single steps outlined
#' in \code{\link{SpatialPolygons}}.
#'
#' @param coords 2-column \code{numeric} matrix with x and y coordinates.
#' @param hole \code{logical}, see \code{\link{Polygon}}.
#' @param ID \code{character}, see \code{\link{Polygons}}.
#' @param data \code{data.frame} with data to add to the output
#' \code{SpatialPolygons*} object (optional).
#' @param match.ID \code{logical}, see \code{\link{SpatialPolygonsDataFrame}}.
#' @param ... Further arguments passed on to \code{\link{SpatialPolygons}}
#' (i.e., \code{p0} and \code{proj4string}).
#'
#' @return If \code{data} is missing, a \code{SpatialPolygons} object; else a
#' \code{SpatialPolygonsDataFrame} object.
#'
#' @seealso
#' \code{\link{SpatialPolygons-class}}, \code{\link{SpatialPolygonsDataFrame}}.
#'
#' @examples
#' library(sp)
#'
#' coords1 <- cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2))
#' spy1 <- coords2Polygons(coords1, ID = "A")
#'
#' coords2 <- cbind(c(5, 4, 2, 5), c(2, 3, 2, 2))
#' spy2 <- coords2Polygons(coords2, ID = "B")
#'
#' plot(spy1, col = "grey75")
#' plot(spy2, col = "grey25", add = TRUE)
#'
#' @export coords2Polygons
#' @name coords2Polygons
coords2Polygons <- function(coords, hole = NA, ID, data, match.ID = TRUE, ...) {

  # convert coordinates matrix to 'Polygons' object
  py <- sp::Polygon(coords, hole)
  pys <- sp::Polygons(list(py), ID)

  # 'Polygons' to 'SpatialPolygons'
  spy <- sp::SpatialPolygons(list(pys), ...)

  if (!missing("data")) {
    return(sp::SpatialPolygonsDataFrame(spy, data, match.ID))
  } else {
    return(spy)
  }
}
