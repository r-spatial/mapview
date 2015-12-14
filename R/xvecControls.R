# Convenience functions for working with spatial objects and xvec maps

## convert sp objects to dummy dataframes
toSPDF <- function(x) {
  cls <- class(x)[1]
  newcls <- paste0(cls, "DataFrame")
  if (cls %in% "SpatialPolygons") {
    x <- as(x, newcls)
  }

  if (cls %in% c("SpatialPoints", "SpatialLines")) {
    x <- as(x, newcls)
    x@data <- data.frame(dummy = rep(0, length(x)))
  }

  return(x)

}

