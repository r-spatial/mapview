### TO DO
### function addExtent()
### function showExtent(map = NULL)
##### which then uses initBasemaps() and addExtent()

#' view spatial objects interactively
#'
#' @export addExtent
#' @name addExtent
#' @rdname addExtent
#' @aliases addExtent


## Extent ================================================================

addExtent <- function(x,
                      map = NULL,
                      map.types = c("OpenStreetMap",
                                    "Esri.WorldImagery"),
                      verbose = FALSE,
                      ...) {

  if (!identical(projection(x), leaflet:::epsg4326)) {
    x <- raster::projectRaster(x, crs = leaflet:::epsg4326)
  }

  ext <- raster::extent(x)

  m <- initBaseMaps(map.types)
  m <- leaflet::addRectangles(m,
                              lng1 = ext@xmin,
                              lat1 = ext@ymin,
                              lng2 = ext@xmax,
                              lat2 = ext@ymax,
                              ...)

  return(m)
}

