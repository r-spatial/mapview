#' View extent/bbox of spatial objects interactively
#'
#' @description
#' This function produces an interactive GIS-like view of the extent/bbox
#' of the supplied spatial object
#'
#' @param x either a Raster* object or a Spatial* object
#' @param map a leaflet map the extent should be added to. If NULL
#' standard background layers are cretaed
#' @param map.types the map types to be used in case map is NULL
#' @param popup a character vector of the HTML content for the popups. See
#' \code{\link{addControl}} for details.
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
#'
#' viewExtent(meuse.grid)
#'
#' @export viewExtent
#' @name viewExtent
#' @aliases viewExtent,addExtent
#'
NULL

## View Extent ============================================================
#' @rdname viewExtent
viewExtent <- function(x,
                       map = NULL,
                       map.types = c("OpenStreetMap",
                                     "Esri.WorldImagery"),
                       popup = NULL,
                       ...) {

  m <- initMap(map, map.types, projection(x))

  grp <- deparse(substitute(x))
  grp <- paste(grp, "extent", sep = "_")

  addex <- addExtent(x, map = m, group = grp, popup = popup, ...)
  m <- addex$map
  out_obj <- list(addex$obj)

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  out <- new('mapview', object = out_obj, map = m)

  return(out)

}

## Add Extent =============================================================
#' @describeIn viewExtent

addExtent <- function(x, map, popup, ...) {

  llcrs <- "+proj=longlat +datum=WGS84 +no_defs"

  spclss <- c("SpatialPointsDataFrame",
              "SpatialPolygonsDataFrame",
              "SpatialLinesDataFrame",
              "SpatialPoints",
              "SpatialPolygons",
              "SpatialLines")
  sptrue <- any(class(x)[1] %in% spclss)

  rstclss <- c("SpatialPixelsDataFrame",
               "RasterLayer",
               "RasterStack",
               "RasterBrick")
  rsttrue <- any(class(x)[1] %in% rstclss)

  pop.null <- is.null(popup)

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

  title <- "EXTENT (EPSG:4326):"
  txt_xmin <- paste0("xmin: ", round(ext@xmin, 5))
  txt_xmax <- paste0("xmax: ", round(ext@xmax, 5))
  txt_ymin <- paste0("ymin: ", round(ext@ymin, 5))
  txt_ymax <- paste0("ymax: ", round(ext@ymax, 5))

  if (pop.null) popup <- paste(title, txt_xmin, txt_xmax,
                               txt_ymin, txt_ymax, sep = "<br/>")

  m <- leaflet::addRectangles(map = map,
                              lng1 = ext@xmin,
                              lat1 = ext@ymin,
                              lng2 = ext@xmax,
                              lat2 = ext@ymax,
                              popup = popup,
                              ...)

  return(list(obj = ext, map = m))
}

