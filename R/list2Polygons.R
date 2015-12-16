# ## require packages
# library(rgdal)
# library(fastshp)
#
# ## import .shp
# #spy_ch <- read.shp("buildings.shp")
#
# ## convert list entries to 'SpatialPolygons' objects
# list2Polygons <- function(x, spatial = FALSE, ...) {
#   spy <- sp::Polygons(list(sp::Polygon(coords = cbind(x$x, x$y))), ID = x$id)
#
#   if (spatial)
#     spy <- sp::SpatialPolygons(list(spy), ...)
#
#   return(spy)
# }
#
# lst_pys <- lapply(spy_ch[1:10], function(i) {
#   list2Polygons(i, spatial = TRUE, proj4string = CRS("+init=epsg:4326"))
# })
#
