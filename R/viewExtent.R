#' View extent/bbox of spatial objects interactively
#'
#' @description
#' This function produces an interactive view of the extent/bbox
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
                       map.types = mapviewGetOption("basemaps"),
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

  out <- methods::new('mapview', object = out_obj, map = m)

  return(out)

}

## Add Extent =============================================================
#' @describeIn viewExtent add extent/bbox of spatial objects interactively

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

  df <- data.frame(xmin = round(ext@xmin, 7),
                   xmax = round(ext@xmax, 7),
                   ymin = round(ext@ymin, 7),
                   ymax = round(ext@ymax, 7))

  mat <- df2String(df)
  cols <- colnames(df)

  ## create list with row-specific html code
  if (pop.null) popup <- listPopupTemplates(mat, cols,
                                            system.file("templates/popup.brew",
                                                        package = "mapview"))

  m <- leaflet::addRectangles(map = map,
                              lng1 = ext@xmin,
                              lat1 = ext@ymin,
                              lng2 = ext@xmax,
                              lat2 = ext@ymax,
                              popup = popup,
                              ...)

  return(list(obj = ext, map = m))
}



### extent without crs ====================================================

viewExtentNoRef <- function(x,
                            popup = NULL,
                            ...) {

  ext <- raster::extent(x)

  m <- leaflet::leaflet()
  m <- leaflet::addRectangles(map = m,
                              lng1 = ext@xmin,
                              lat1 = ext@ymin,
                              lng2 = ext@xmax,
                              lat2 = ext@ymax,
                              popup = popup,
                              ...)

  out <- methods::new('mapview', object = list(ext), map = m)

  return(out)
}


