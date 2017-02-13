
### MISC ==================================================================
sf2DataFrame <- function(x, remove_sf_column = FALSE) {
  stopifnot(inherits(x, "sf") | inherits(x, "sfc"))
  if (inherits(x, "sf")) {
    if (remove_sf_column) {
      geompos <- which(names(x) == attr(x, "sf_column"))
      return(data.frame(x)[, -geompos, drop = FALSE])
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


npts = function(x) {
  if (getGeometryType(x) == "pt") {
    length(sf::st_geometry(x))
  } else {
    nNodes(sf::st_geometry(x))
  }
}

#100000 / (npts(st_geometry(st_as_sf(gadmCHE))) / length(st_geometry(st_as_sf(gadmCHE))))
