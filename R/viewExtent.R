#' View extent/bbox of spatial objects interactively
#'
#' @description
#' This function produces an interactive view of the extent/bbox
#' of the supplied spatial object
#'
#' @param x either a Raster*, sf* or Spatial* object
#' @param data either a Raster*, sf* or Spatial* object
#' @param map a leaflet map the extent should be added to. If NULL
#' standard background layers are cretaed
#' #' @param maxpixels integer > 0. Maximum number of cells to use for the plot.
#' If maxpixels < \code{ncell(x)}, sampleRegular is used before plotting.
#' @param color color (palette) for points/polygons/lines
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param alpha opacity of the lines or points
#' @param alpha.regions opacity of the fills or the raster layer(s)
#' @param layer.name the name of the layer to be shown on the map
#' @param homebutton logical, whether to add a zoom-to-layer button to the map.
#' Defaults to TRUE
#' @param popup a \code{list} of HTML strings with the popup contents, usually
#' created from \code{\link{popupTable}}. See \code{\link{addControl}} for
#' details.
#' @param native.crs logical whether to reproject to web map coordinate
#' reference system (web mercator - epsg:3857) or render using native CRS of
#' the supplied data (can also be NA). Default is FALSE which will render in
#' web mercator. If set to TRUE now background maps will be drawn (but rendering
#' may be much quicker as no reprojecting is necessary). Currently only works
#' for simple features.
#' @param ... additional arguments passed on to \code{\link{addRectangles}}
#'
#' @author
#' Tim Appelhans
#'
#' @examples
#' \dontrun{
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
#' }
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
                       map.types = NULL,
                       popup = NULL,
                       lwd = 1,
                       alpha = 0.9,
                       alpha.regions = 0.1,
                       color = "#6666ff",
                       homebutton = TRUE,
                       layer.name = deparse(substitute(x)),
                       native.crs = FALSE,
                       ...) {

  if (is.null(map.types)) map.types <- basemaps(color)
  m <- initMap(map, map.types, getProjection(x), native.crs)
  color <- col2Hex(color)

  layer.name <- paste(layer.name, "extent", sep = "-")

  m <- addExtent(map = m,
                 data = x,
                 group = layer.name,
                 popup = popup,
                 weight = lwd,
                 opacity = alpha,
                 fillOpacity = alpha.regions,
                 color = color,
                 ...)

  m <- decorateMap(map = m,
                   funs = list(if (!native.crs) leaflet::addScaleBar,
                               if (homebutton) addHomeButton,
                               mapViewLayersControl,
                               addMouseCoordinates),
                   args = list(if (!native.crs) list(position = "bottomleft"),
                               if (homebutton) list(ext = createExtent(x),
                                                    layer.name = layer.name),
                               list(map.types = map.types,
                                    names = layer.name,
                                    native.crs = native.crs),
                               list(style = "detailed",
                                    epsg = NULL,
                                    proj4string = getProjection(x))))

  out <- methods::new('mapview',
                      object = list(createExtent(x, offset = 0)),
                      map = m)

  return(out)

}

## Add Extent =============================================================
#' @describeIn viewExtent add extent/bbox of spatial objects interactively
#' @export addExtent

addExtent <- function(map, data, popup = NULL, ...) {

  x <- checkAdjustProjection(data)
  ext <- createExtent(x, offset = 0)
  df <- data.frame(xmin = ext@xmin,
                   xmax = ext@xmax,
                   ymin = ext@ymin,
                   ymax = ext@ymax)

  if (is.null(popup)) popup <- popupTable(df)

  m <- leaflet::addRectangles(map = map,
                              lng1 = ext@xmin,
                              lat1 = ext@ymin,
                              lng2 = ext@xmax,
                              lat2 = ext@ymax,
                              popup = popup,
                              ...)
  return(m)

}



# ### extent without crs ====================================================
#
# viewExtentNoRef <- function(x,
#                             popup = NULL,
#                             ...) {
#
#   ext <- raster::extent(x)
#
#   m <- leaflet::leaflet()
#   m <- leaflet::addRectangles(map = m,
#                               lng1 = ext@xmin,
#                               lat1 = ext@ymin,
#                               lng2 = ext@xmax,
#                               lat2 = ext@ymax,
#                               popup = popup,
#                               ...)
#
#   out <- methods::new('mapview', object = list(ext), map = m)
#
#   return(out)
# }
#

