if (!isGeneric('localTiles3031')) {
  setGeneric('localTiles3031', function(x, ...)
    standardGeneric('localTiles3031'))
}
#' create epsg3031 projected local tiles for leaflet and maps it
#'
#' @description localTiles3031 creates and maps a tiles from a given raster or
#'   GDAL object. Up to know there is no way to initialize a htmlwidget object
#'   dynamically. So we have to create static versions for the region on higher
#'   latitude than +/- 85 degree
#' @usage servr::httd("~/proj/makeTile/etopo/tiles",daemon=TRUE)
#' localTiles3031(x="http://localhost:4321/{z}/{x}/{y}.png")
#' @param x  list of local/remote urls pointing to the tiles to serve
#' @param zoom number of tile levels dont count the zero level
#' @param xname list of layer names
#' @param tileSize sie of the tiles in pixel default is 256,
#' @param attribution of the layer
#'
#' @author Chris Reudenbach
#'
#' @examples
#' \dontrun{
#'  ### due to a lot of incompatibiliets it is necesary to serve the local tiles aat local host
#'  ### we just use the servr package to do so.
#'   require(servr)
#'   # we serve the directory that we did use with see \code{\link{makeTile}}
#'
#'   servr::httd("~/proj/makeTile/etopo/tiles",daemon=TRUE)
#'   # and we map it
#'   localTiles3031(x=c("http://localhost:4321/{z}/{x}/{y}.png","http://localhost:4321/{z}/{x}/{y}.png"), zoom=5, xname=c("BaseTile","overlaytile"))


#'}
#'@name localTiles3031
#'@export localTiles3031
#'@rdname localTiles3031

library(raster)


localTiles3031<- function(x=NULL,
                          xname='LocalRasterTile',
                          tileSize=256,
                          attribution=NULL,
                          zoom=7)
  {

  if (is.null(attribution)) {attribution <-"<a href='https://wiki.earthdata.nasa.gov/display/GIBS'> NASA EOSDIS GIBS</a> &nbsp;|| &nbsp; <a href='https://github.com/kartena/Proj4Leaflet'> Proj4Leaflet</a> | Projection: <a href='http://spatialreference.org/ref/epsg/wgs-84-antarctic-polar-stereographic/'> EPSG3031</a>"}

  epsg3031Code <- "urn:ogc:def:crs:EPSG::3031"
  epsg3031String <- "+proj=stere +lat_0=-90 +lat_ts=-71 +lon_0=0 +k=1 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"


  layer.name = deparse(substitute(x,env = parent.frame()))
  use.layer.names =TRUE

# create list of user data that is passed to the widget
# lst_x <- list(layer = x,
#               data  = 'undefined',
#               layername = layer.name,
#               zoom = zoom)
lst_x <- list(data  = 'undefined',
              layer = x,
              layername=xname,
              zoom = zoom,
              epsgcode=epsg3031Code,
              epsgproj=epsg3031String,
              tilesize=tileSize,
              attribution=attribution)

# creating the widget
localTiles3031Internal(jFn = pathRasFn ,  x = lst_x)

}

### bViewInternal creates fpView widget =================================================

localTiles3031Internal <- function(jFn = NULL, x = NULL) {

  sizing = htmlwidgets::sizingPolicy(
    browser.fill = TRUE,
    viewer.fill = TRUE,
    viewer.padding = 5
  )
  # create widget
  htmlwidgets::createWidget(
    name = 'localTiles3031',
    x,
    sizingPolicy = sizing,
    package = 'mapview'
  )
}

### Widget output function for use in Shiny =================================================
#
localTiles3031Output <- function(outputId, width = '100%', height = '800px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'localTiles3031', width, height, package = 'mapview')
}

### Widget render function for use in Shiny =================================================
#
renderlocalTiles3031<- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, localTiles3031Output, env, quoted = TRUE)
}

### makepath creates temp paths and filenames =================================================
makeTmpPath <- function (p=NULL){
  if (is.null(p)){
    tmpPath <- tempfile()
  } else {tmpPath <- p}
  dir.create(tmpPath)
  dir.create(paste0(tmpPath,"/tiles"))
  baseFn <- "rawTile"
  extFn <- "JPEG"
  rasFn <- paste0(baseFn,".",extFn)
  pathRasFn <- paste0(tmpPath,"/",rasFn)
  return(list(tmpPath,pathRasFn,rasFn,extFn))
}

