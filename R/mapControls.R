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


scaleCoordinates <- function(x, y) {

  ratio <- diff(range(y)) / diff(range(x))
  x_mn <- x - min(x, na.rm = TRUE)
  x_sc <- x_mn / max(x_mn, na.rm = TRUE)
  y_mn <- y - min(y, na.rm = TRUE)
  y_sc <- y_mn / max(y_mn, na.rm = TRUE) * ratio

  return(cbind(x_sc, y_sc))

}


scalePolygonsCoordinates <- function(x) {

  coord_lst <- lapply(slot(x, "polygons"), function(x) {
    lapply(slot(x, "Polygons"), function(y) slot(y, "coords"))
  })

  xcoords <- do.call("c", do.call("c", lapply(seq(coord_lst), function(i) {
    lapply(seq(coord_lst[[i]]), function(j) {
      coord_lst[[i]][[j]][, 1]
    })
  })))

  ycoords <- do.call("c", do.call("c", lapply(seq(coord_lst), function(i) {
    lapply(seq(coord_lst[[i]]), function(j) {
      coord_lst[[i]][[j]][, 2]
    })
  })))

  ratio <- diff(range(ycoords)) / diff(range(xcoords))

  x_mn <- min(xcoords, na.rm = TRUE)
  x_mx <- max(xcoords - min(xcoords, na.rm = TRUE), na.rm = TRUE)

  y_mn <- min(ycoords, na.rm = TRUE)
  y_mx <- max(ycoords - min(ycoords, na.rm = TRUE), na.rm = TRUE)

  for (j in seq(coord_lst)) {
    for (h in seq(coord_lst[[j]])) {
      slot(x@polygons[[j]]@Polygons[[h]], "coords") <-
        cbind((coordinates(x@polygons[[j]]@Polygons[[h]])[, 1] - x_mn) / x_mx,
              (coordinates(x@polygons[[j]]@Polygons[[h]])[, 2] - y_mn) / y_mx)
    }
  }

  return(x)

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
