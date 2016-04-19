#' @export
#'
print.mapview <- function(x, ...) {
  htmlwidgets:::print.htmlwidget(mapview2leaflet(x), ...)
}



### mapview to leaflet ----------------------------------------------------
mapview2leaflet <- function(x) {
  slot(x, "map")
}


### mapview simple class
getSimpleClass <- function(obj) {
  if (class(obj) %in% c("RasterLayer", "RasterStack",
                      "RasterBrick", "Satellite",
                      "SpatialGridDataFrame",
                      "SpatialPixelsDataFrame")) "rst" else "vec"
}


### labels
makeLabels <- function(col) {
  as.character(col)
}


### burst
burst <- function(x, zcol, ...) {

  lst <- lapply(seq(zcol), function(i) {
    if (!is.factor(x@data[, zcol[i]])) {
      x@data[, zcol[i]] <- as.factor(x@data[, zcol[i]])
    }
    f <- x@data[, zcol[i]]
    ls_out <- split(x[, zcol[i]], f, ...)
    names(ls_out) <- paste(zcol[i], names(ls_out), sep = "_")
    return(ls_out)
  })

  names(lst) <- zcol

  if (length(lst) > 1) {
    ls <- lst[[1]]
    for (i in 2:length(lst)) ls <- append(ls, lst[[i]])
  } else {
    ls <- lst[[1]]
  }

  return(ls)
}
