if (!isGeneric('testprj4')) {
  setGeneric('testprj4', function(x, ...)
    standardGeneric('testprj4'))
}
#' test some widget things
#'
#' @description testproj4takes
#'
#' @param x  \code{\link{sp}} SpatialPointDataframe object
#' @param color colors as:  (green,red,blue,teal,yellow,random) for the points/polygons/lines
#' @param x a \code{\link{raster}}* object
#' @param color color (palette) of the points/polygons/lines/pixels
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param alpha opacity of the raster layer(s) (not implemented yet)
#' @param legend should a legend be plotted (not implemented yet)
#' @param legend.opacity opacity of the legend (not implemented yet)
#' @param ... additional arguments passed on to repective functions.
#' See \code{\link{addRasterImage}}, \code{\link{addCircles}},
#' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
#' @author
#' Chris Reudenbach
#'
#' @examples
#' \dontrun{
#' #we need sp and raster ###
#'  library(sp)
#'  library(raster)
#' ### Now we go  bigger
#' ### get some SRTM data
#'   r<-getGeoData(name="SRTM",extent(83.8,27,9,99,34),download=TRUE)
#   prj3035<-"proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#   mapview:::testprj4(x="mosaicSRTM.tif",outPath="/home/creu/proj/testprj4", t_srs= prj3035,scale=c(1000,5000))
#'}
#'@name testprj4
#'@export testprj4
#'@rdname testprj4

library(raster)


testprj4<- function(x=NULL,
                     outPath=tmpDir(),
                     s_srs="+proj=longlat +datum=WGS84",
                     t_srs="+proj=longlat +datum=WGS84")
  {

  # calculate zoom level from extent of input raster
  zoom<-7

  leaf.height <- mapviewGetOption("leafletHeight")
  leaf.hidth <- mapviewGetOption("leafletWidth")
  map.types = mapviewGetOption("basemaps")
  layer.name = deparse(substitute(x,env = parent.frame()))
  use.layer.names =TRUE
  map= NULL
 # m <- mapview:::initMap(map,map.types,"+proj=longlat +datum=WGS84")#crs@projargs)

#  if (use.layer.names) {
#    grp <- names(x)
#  } else {
#    grp <- layer.name
#  }


#  map=NULL
#  m %>% mapview:::mapViewLayersControl(map = m,
#                                       map.types = map.types
#  )
#  mapview:::tileDependencies()
#   m = leaflet() %>% addTiles()
#   m %>% fitBounds(0, 40, 10, 50)
#   m %>% leaflet::addTiles(paste0("/home/creu/proj/testprj4","/tiles/{z}/{x}/{y}.png"),tileOptions(tms = FALSE,
#                                                                                minZoom= 0,
#                                                                                maxZoom= 6,
#                                                                                continuousWorld= FALSE,
#                                                                                noWrap= TRUE))


# create list of user data that is passed to the widget
lst_x <- list(layer = x,
              data  = 'undefined',
              layername = layer.name,
              zoom = zoom)

# creating the widget
testprj4Internal(jFn = pathRasFn ,  x = lst_x)

}

### bViewInternal creates fpView widget =================================================

testprj4Internal <- function(jFn = NULL, x = NULL) {

  sizing = htmlwidgets::sizingPolicy(
    browser.fill = TRUE,
    viewer.fill = TRUE,
    viewer.padding = 5
  )
  # create widget
  htmlwidgets::createWidget(
    name = 'testprj4',
    x,
    sizingPolicy = sizing,
    package = 'mapview'
  )
}

### Widget output function for use in Shiny =================================================
#
testprj4Output <- function(outputId, width = '100%', height = '800px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'testprj4', width, height, package = 'mapview')
}

### Widget render function for use in Shiny =================================================
#
rendertestproj4<- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, testprj4Output, env, quoted = TRUE)
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




#r<- system(paste0("gdalwarp -s_srs EPSG:4326 -t_srs '",projlatlon,"' -r bilinear -overwrite  -of GTiFF ",r@file@name, paste0(" ",tmpPath,"/",rasFn)),intern=T)
#system(paste("listgeo -tfw ", fnx@file@name))

#r <-  system(paste0("gdal_translate  -scale -of ", rasType," ",  fnx@file@name," ",paste0(tmpPath,"/",rasFn)),intern=T)

# x <- raster::projectRaster(
#  ras, raster::projectExtent(ras, crs = sp::CRS(wmcrs)),
#  method = "bilinear")
# writeRaster(x,paste0(tmpPath,"/",rasFn))

# get rid of aux.xml file
# if (file.exists(paste0(tmpPath,"/",rasFn,".aux.xml"))) r <- file.remove (paste0(tmpPath,"/",rasFn,".aux.xml"))
#proj4<- gdalinfo(pathRasFn,proj4 = TRUE)[24]