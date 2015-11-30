if (!isGeneric('bView')) {
  setGeneric('bView', function(data,col,width,height,zcol,map,burst,radius,map.types,legend,legend.opacity,verbose,layer.name,popup , ...)
    standardGeneric('bView'))
}

#' Leaflet maps for big line and polygon data
#'
#' @description bView is a first prototype for rendering big data (lines and polygons) on base of leaflet maps utilizing rtree, HTML5 canvas and htmlwidgets.
#'
#' This is a modified and adapted implementation that uses the concepts and code of Sumbera \url{https://gist.github.com/Sumbera/c67e5551b21c68dc8299} and Oscar Hidalgo \url{http://www.cyoarte.com/i+d/cphp/}
#'
#'@note It is somehow important to understand the rendering concept. Due to the huge data sets the data is rendered using the canvas concept of HTML5. This is very effective and fast but the data is not responsive (maybe this will be implemented in a second step). For getting informations by click on the features you usually have to zoom in until the features turn to magenta. At this point you have full access to the vectors as they are provided by an rtree concept. Not perfect but it works fully on the client side of the browser. The most time consuming part is the conversion of the data to the geojson format.
#'
#'
#'
#' @param data a \code{\link{sp}} SpatialPolyDataframe object

#'
#' @author
#' Chris Reudenbach
#' @examples
#'
#' ### we need sp, raster, gdalUtils and rgdal ###
#'  library(sp)
#'  library(raster)
#'  library("rgdal")
#'  library("gdalUtils")
#' ### for downloading and benchmarking
#'  library(downloader)
#'  library(profvis)
#'
#' ## load gadmCHE example data
#'  data(gadmCHE)
#'
#' ## map it with mapview
#'  mapview(gadmCHE)
#' ## map it with bView
#'  bView(gadmCHE)
#'
#' ###  to get more suisse data we use OSM as provided by geofabrik
#'  download("http://download.geofabrik.de/europe/switzerland-latest.shp.zip",dest="switzerland.zip", mode = "wb")
#'  unzip ("switzerland.zip",exdir = "./")
#'
#' ## get information of landuse
#'  ogrInfo(".", "landuse")
#'
#' ## put it in sp object (doesn't matter what type)
#'  landuseCH<- readOGR(".","landuse")
#'
#' ## map it with mapview
#'  mapview(landuseCH)
#' ## map it with bView
#'  bView(landuseCH)
#'
#' ## get information of waterways
#'  ogrInfo(".", "waterways")
#'
#' ## read it to a sp object
#'  waterwaysCH<- readOGR(".","waterways")
#'
#' ## map it with mapview
#'  mapview(waterwaysCH)
#' ## map it with bView
#'  bView(waterwaysCH)

#' ### some benchmarks
#'  system.time(mapview(gadmCHE))
#'  system.time(bView(gadmCHE))
#'  system.time(mapview(landuseCH))
#'  system.time(bView(landuseCH))
#'  system.time(mapview(waterwaysCH))
#'  system.time(bView(waterwaysCH))
#'

#'
#' @export
#' @docType bView
#' @name bView
#' @rdname bView
#'
#'
## SpatialPointsDataFrame =================================================



bView <- function(data,
                  col = 'rgb',
                  width = NULL,
                  height = NULL,
                  zcol = NULL,
                  map = NULL,
                  burst = FALSE,
                  radius = 10,
                  map.types = c("OpenStreetMap", "Esri.WorldImagery",'Thunderforest.Landscape'),
                  legend = FALSE,
                  legend.opacity = 1,  verbose = mapviewOptions(console = FALSE)$verbose,
                  layer.name = deparse(substitute(data,
                                                  env = parent.frame())),
                  popup = NULL,  ...) {
  # check if a sp object exist
  library("gdalUtils")
  library("rgdal")

  libpath<- .libPaths()
  dataToLibPath<- paste0(libpath[1],"/mapview/htmlwidgets/lib/data")
  jsonFn <- paste0(dataToLibPath,"/data.json")
  shpFn <- paste0(dataToLibPath,"/data.shp")
  if (!is.null(data)) {
    data.latlon <- spTransform(data,CRS("+init=epsg:4326"))
    writeOGR(data.latlon, dsn = dataToLibPath, layer = "data", driver="ESRI Shapefile", overwrite_layer = TRUE)
    ogr2ogr(src_datasource_name = shpFn, dst_datasource_name = jsonFn, f = "GeoJSON", overwrite = TRUE)
    # wrap it with var data = {};
    lns <- readLines(jsonFn)
    lns[1] <- 'var data = {'
    lns[length(lns)]<- '};'
    writeLines(lns, jsonFn)

    # we need scale and zoom so we approximate the area and zoom factor
    ext <- extent(data.latlon)
    yc <- (ext@ymax-ext@ymin) * 0.5  + ext@ymin
    xc <- (ext@xmax-ext@xmin) * 0.5 + ext@xmin

    rad.cof=3.1459/180
    lat.1deg=110540
    lon.1deg=111320*cos(rad.cof*yc)
    # calculate stepsize
    latextent=(lat.1deg*(ext@ymax-ext@ymin))
    lonextent=(lon.1deg*(ext@xmax-ext@xmin))

    #http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Resolution_and_Scale
    zoomlevel <- 3
    repeat{
      # res in m zoomlev is 2 ^ x
      res <- 156543.03  * cos(rad.cof*yc) / (2 ^ zoomlevel)
      #calculating screen scale assuming screen 96 dpi in/m 1000/25.4
      scale = 96 * 39.37 * res
      if(scale < max(latextent, 2500000)){
        break
      }
      zoomlevel <- zoomlevel + 1
    }
  } else {
    NULL
  }



  # create list of user data that is passed to the widget
  x = list(color <- col,
           layer <- map.types,
           data  <- 'undefined',
           cnames <- 'undefined',
           centerLat <- yc,
           centerLon <- xc,
           zoom <- zoomlevel)

  # create widget
  htmlwidgets::createWidget(
    name = 'bView',
    x,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.fill = TRUE,
      viewer.fill = TRUE,
      viewer.padding = 20

    ),
    package = 'mapview'
  )

}

#' Widget output function for use in Shiny
#'
#' @export
bViewOutput <- function(outputId, width = '100%', height = '400px') {
  shinyWidgetOutput(outputId, 'bView', width, height, package = 'mapview')
}

#' Widget render function for use in Shiny
#'
#' @export
renderbView <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  shinyRenderWidget(expr, bViewOutput, env, quoted = TRUE)
}
