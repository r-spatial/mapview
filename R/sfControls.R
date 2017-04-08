
### MISC ==================================================================
sf2DataFrame <- function(x, drop_sf_column = FALSE) {
  stopifnot(inherits(x, "sf") | inherits(x, "sfc"))
  if (inherits(x, "sf")) {
    if (drop_sf_column) {
      return(as.data.frame(x)[setdiff(names(x), attr(x, "sf_column"))])
      # geompos <- which(names(x) == attr(x, "sf_column"))
      # return(data.frame(x)[, -geompos, drop = FALSE])
    } else return(x)
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

#' count the number of points/vertices/nodes of sf objects
#' @param x an sf/sfc object
#'
#' @export
#'
#' @examples
#' npts(franconia)
#' npts(sf::st_geometry(franconia[1, ])) # first polygon
#'
#' npts(breweries) # is the same as
#' nrow(breweries)
#'
npts = function(x) {
  if (getGeometryType(x) == "pt") {
    length(sf::st_geometry(x))
  } else {
    nNodes(sf::st_geometry(x))
  }
}
