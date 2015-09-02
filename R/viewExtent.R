#' view extent/bbox of spatial objects interactively
#'
#' @description
#' this function produces an interactive GIS-like view of the extent/bbox
#' of the supplied spatial object
#'
#' @param x either a Raster* object or a Spatial* object
#' @param map a leaflet map the extent should be added to. If NULL
#' standard background layers are cretaed
#' @param map.types the map types to be used in case map is NULL
#' @param ... additional arguments passed on to \code{\link{addRectangles}}
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' ### raster data ###
#' library(sp)
#' library(raster)
#'
#' data(meuse.grid)
#' coordinates(meuse.grid) = ~x+y
#' proj4string(meuse.grid) <- CRS("+init=epsg:28992")
#' gridded(meuse.grid) = TRUE
#' meuse_rst <- stack(meuse.grid)
#'
#' m1 <- mapView(meuse_rst[[1]])
#' viewExtent(meuse_rst, map = m1)
#'
#' @export viewExtent
#' @name viewExtent
#' @rdname viewExtent
#' @aliases viewExtent,addExtent
#'

## view Extent ============================================================
viewExtent <- function(x,
                       map = NULL,
                       map.types = c("OpenStreetMap",
                                     "Esri.WorldImagery"),
                       ...) {

#   ## create base map using specified map types
#   if (is.null(map)) {
#     m <- initBaseMaps(map.types)
#   } else {
#     m <- map
#   }

  m <- initMap(map, map.types, projection(x))

  nam <- sys.call(0)
  grp <- as.character(nam)[2]
  grp <- paste("extent", grp)

  m <- addExtent(x, map = m, group = grp, ...)

  ## add layer control buttons
  if (is.null(map)) {
    m <- leaflet::addLayersControl(map = m,
                                   position = "topleft",
                                   baseGroups = map.types,
                                   overlayGroups = grp)
  } else {
    m <- leaflet::addLayersControl(map = m,
                                   position = "topleft",
                                   baseGroups = map.types,
                                   overlayGroups =
                                     c(getLayerNamesFromMap(m),
                                       grp))
  }

  return(m)

}

## add Extent =============================================================
#' @describeIn viewExtent
#'
addExtent <- function(x, map, ...) {

  llcrs <- "+proj=longlat +datum=WGS84 +no_defs"

  spclss <- c("SpatialPointsDataFrame",
              "SpatialPolygonsDataFrame",
              "SpatialLinesDataFrame",
              "SpatialPoints",
              "SpatialPolygons",
              "SpatialLines")
  sptrue <- any(class(x)[1] %in% spclss)

  rstclss <- c("RasterLayer",
               "RasterStack",
               "RasterBrick")
  rsttrue <- any(class(x)[1] %in% rstclss)

  if (sptrue) {
    if (!identical(projection(x), llcrs)) {
      x <- sp::spTransform(x, CRSobj = llcrs)
    }
  } else if (rsttrue) {
    if (!identical(projection(x), llcrs)) {
      x <- raster::projectExtent(x, crs = llcrs)
    }
  } else stop(paste("need one of", paste(spclss, collapse = " or "), "or",
                    paste(rstclss, collapse = " or "), "to draw extent"))

  ext <- raster::extent(x)

  txt_xmin <- paste0("xmin: ", round(ext@xmin, 5))
  txt_xmax <- paste0("xmax: ", round(ext@xmax, 5))
  txt_ymin <- paste0("ymin: ", round(ext@ymin, 5))
  txt_ymax <- paste0("ymax: ", round(ext@ymax, 5))

  txt <- paste(txt_xmin, txt_xmax, txt_ymin, txt_ymax, sep = "<br/>")

  m <- leaflet::addRectangles(map = map,
                              lng1 = ext@xmin,
                              lat1 = ext@ymin,
                              lng2 = ext@xmax,
                              lat2 = ext@ymax,
                              popup = txt,
                              ...)

  return(m)
}

