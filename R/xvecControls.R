# Convenience functions for working with spatial objects and xvec maps

## convert sp objects to dummy dataframes
toSPDF <- function(x) {
  cls <- class(x)[1]
  if (cls %in% c("SpatialPoints", "SpatialPolygons", "SpatialLines")) {
    newcls <- paste0(cls, "DataFrame")
    x <- as(x, newcls)
  }
  return(x)

}

