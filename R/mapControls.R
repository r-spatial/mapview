#' convenience functions for working with leaflet maps
#'
#' @param m a leaflet map
#'
#' @author
#' Tim Appelhans
#'

getLayerControlEntriesFromMap <- function(m) {

  seq_along(m$x$calls)[sapply(m$x$calls,
                              FUN = function(X) "addLayersControl" %in% X)]

}


getLayerNamesFromMap <- function(m) {

  len <- getLayerControlEntriesFromMap(m)
  len <- len[length(len)]
  if (length(len) != 0) m$x$calls[[len]]$args[[2]] else NULL

}


layers2bHidden <- function(m) {

  nms <- getLayerNamesFromMap(m)
  nms[2:length(nms)]

}



projectRasterForMapView <- function(x) {
  #epsg4326 <- "+proj=longlat +datum=WGS84 +no_defs"
  wmerc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs"
  is.fact <- raster::is.factor(x)
  if (is.fact) {
    out_rst <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(wmerc)),
      method = "ngb")
    out_rst <- raster::as.factor(out_rst)
  } else {
    out_rst <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(wmerc)),
      method = "bilinear")
  }

  return(out_rst)

}


initBaseMaps <- function(map.types) {
  ## create base map using specified map types
    m <- leaflet::leaflet()
    m <- leaflet::addProviderTiles(m, provider = map.types[1],
                                   group = map.types[1])
    if (length(map.types) > 1) {
      for (i in 2:length(map.types)) {
        m <- leaflet::addProviderTiles(m, provider = map.types[i],
                                       group = map.types[i])
      }
    }
  return(m)
}

# extractObjectName <- function(x) {
#   pipe_splt <- strsplit(x, "%>%")[[1]][-1]
#
#   grp <- vector("character", length(pipe_splt))
#   for (i in seq(grp)) {
#     x <- pipe_splt[i]
#     tmp <- strsplit(strsplit(x,
#                              "\\(")[[1]][2], ",")[[1]][1]
#     grp[i] <- gsub("\\)", "", tmp)
#   }
#   return(grp)
# }
