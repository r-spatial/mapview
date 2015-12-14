## require packages
library(rgdal)
library(fastshp)

## import .shp
spy_ch <- read.shp("buildings.shp")

## convert list entries to 'SpatialPolygons' objects
list2SpatialPolygons <- function(x, ...) {
  spy <- sp::Polygons(list(sp::Polygon(coords = cbind(x$x, x$y))), ID = x$id)
  spy <- sp::SpatialPolygons(list(spy), ...)

  return(spy)
}

lst_pys <- lapply(spy_ch, function(i) {
  list2SpatialPolygons(i, proj4string = CRS("+init=epsg:4326"))
})
