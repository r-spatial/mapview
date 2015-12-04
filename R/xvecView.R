if (!isGeneric('xVecView')) {
  setGeneric('xVecView', function(x, ...)
    standardGeneric('xVecView'))
}
#' Leaflet maps for big point data
#'
#' @description fpView is designed for rendering fairly big point vector data sets on base of leaflet maps without suffering the typical tough response times of the common leaflet maps. It utilises HTML5 canvas and webGL technics.
#'
#'@references  fpView is using a modified and adapted implementation of : \url{https://github.com/robertleeplummerjr/Leaflet.glify}
#'
#'
#'@note It is import to understand that the accurracy of the rendering is about
#'  1.1 m at the equator up to 20 cm around 75°. You will get an arbitrary result if the
#'   accurracy of your points requires more than 5 decimal digits.
#'
#' @param x  \code{\link{sp}} SpatialPointDataframe object
#' @param color colors as:  (green,red,blue,teal,yellow,random) for the points/polygons/lines
#' @param x a \code{\link{raster}}* object
#' @param color color (palette) of the points/polygons/lines/pixels
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param layer.opacity opacity of the raster layer(s) (not implemented yet)
#' @param legend should a legend be plotted (not implemented yet)
#' @param legend.opacity opacity of the legend (not implemented yet)
#' @param ... additional arguments passed on to repective functions.
#' See \code{\link{addRasterImage}}, \code{\link{addCircles}},
#' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
#' @author
#' Chris Reudenbach
#' @examples
#' ### we need sp and raster ###
#'  library(sp)
#'  library(raster)
#'  library(ggplot2)
#'  library(profvis)
#'
#' ### take the meuse data
#'  data(meuse)
#'  coordinates(meuse) <- ~x+y
#'  proj4string(meuse) <- CRS("+init=epsg:28992")
#'  meuse <- spTransform(meuse,CRS("+init=epsg:3857"))
#'
#' ### map it with mapview
#'  mapview(meuse)
#'
#' ### map it with fpView
#'  fpView(meuse,color = "random")
#'
#' ### some benchmarks
#'  system.time(mapview(meuse))
#'  system.time(fpView(meuse, color = "random"))
#'
#' ### Now we go a bit bigger
#'
#' ### get the diamonds data
#'  big <- diamonds[rep(seq_len(nrow(diamonds)), 1),]
#'  big$cut <- as.character(big$cut)
#'  big$color <- as.character(big$color)
#'  big$clarity <- as.character(big$clarity)
#'
#' ### provide some random positions
#'  big$x <- rnorm(nrow(big), 10, 3)
#'  big$y <- rnorm(nrow(big), 50, 3)
#'  coordinates(big) <- ~x+y
#'  proj4string(big) <- CRS("+init=epsg:4326")
#'
#' ### map it with pure mapview
#'  mapview(big, color = 'blue')
#'
#' ### map it with fastmap
#'  fpView(big, color = 'blue')
#'
#' ### some benchmarks
#'  system.time(mapview(big, color = 'blue'))
#'  system.time(fpView(big, color = 'blue'))
#'
#' ### up to about 5 mio points
#'  big <- diamonds[rep(seq_len(nrow(diamonds)), 94),]
#'  big$cut <- as.character(big$cut)
#'  big$color <- as.character(big$color)
#'  big$clarity <- as.character(big$clarity)
#'  big$x <- rnorm(nrow(big), 10, 3)
#'  big$y <- rnorm(nrow(big), 50, 3)
#'  coordinates(big) <- ~x+y
#'  proj4string(big) <- CRS("+init=epsg:4326")
#'
#' ### map it with fpView
#'  fpView(big, color = "blue")
#'
#' ### some benchmarks
#' # random point colors is slower
#'  system.time(fpView(big, color = "random"))
#' # than unique colors
#'  system.time(fpView(big, color = "blue"))
#' # profVising it
#'  profvis(fpView(big, color = "blue"))
#'
#' @export fpView
#' @docType fpView
#' @name fpView
#' @rdname fpView
#'
#'
fpView <- function(x,
                  zcol = NULL,
                  map = NULL,
                  burst = FALSE,
                  color = mapViewPalette(7),
                  na.color = mapviewGetOption("nacolor"),
                  values = NULL,
                  map.types = mapviewGetOption("basemaps"),
                  legend = FALSE,
                  layer.opacity = 0.8,
                  legend.opacity = 1,
                  weight = 2,
                  verbose = mapviewGetOption("verbose"),
                  layer.name = deparse(substitute(x,
                                                  env = parent.frame())),
                  popup = NULL,
                  ...) {


   data<- x
  ## temp dir
  tmpPath <- makepath()[[1]][1]
  pathJsonFn <- makepath()[[2]][1]
  jsonFn <- makepath()[[3]][1]

# check if a sp object exist
if (!is.null(data)) {
  data.latlon <- spTransform(data,CRS("+init=epsg:4326"))
  df <- as.data.frame(data.latlon)
  drops <- c("x","y")
  df.cols <- df[,!(names(df) %in% drops)]
  cnames <- colnames(df.cols)
  if (!is.null(zcol)) {
    cnames <- zcol
  }
  df.cols <- lapply(df.cols, as.character)
  df.sort <- df[,c("x","y",cnames)]
  #df.sort[is.na(df.sort)] <- -9999
  out.matrix = t(t(df.sort))
  data.json <- coords2JSON(out.matrix)
  # write data file to temp dir
  file.create(pathJsonFn)
  fileConn <- file(pathJsonFn)
  write(data.json, fileConn)
  close(fileConn)

  # estimating the zoomlevel and center point of the map
  zoom<- getZoomLevel(data.latlon)

  # creating the popup names
  cHelp <- list()
  cHelp[1] <- "<tr class='coord'><td>Longitude</td><td>"
  cHelp[2] <- "<tr class='coord'><td>Latitude</td><td>"
  for (i in 1:length(cnames)) {
    if (i %% 2 == 1) {
      cHelp[i + 2] <- paste0("<tr><td> ",cnames[i]," </td><td>")
    } else {
      cHelp[i + 2] <- paste0("<tr class='alt'><td> ",cnames[i]," </td><td>")
    }
  }

  # create list of user data that is passed to the widget
  x = list(
    color <- color,
    layer <- map.types,
    data  <- "undefined",
    cnames <- cnames,
    centerLat <- zoom[3],
    centerLon <- zoom[2],
    zoom <- zoom[1],
    popTemplate <- getStyle(),
    cHelp <- cHelp,
    layer.opacity <- layer.opacity
  )
}
  # creating the widget
  fpViewInternal(jFn = pathJsonFn,  args = x)
}


fpViewInternal <- function(jFn = NULL, args = NULL) {
  x <- list(args = args)
  data_dir <- dirname(jFn)
  data_file <- basename(jFn)
  dep1 <- htmltools::htmlDependency(
    name = "data",
    version = "1",
    src = c(file = data_dir),
    attachment = list(data_file)
  )
  deps <- list(dep1)

  sizing = htmlwidgets::sizingPolicy(
    browser.fill = TRUE,
    viewer.fill = TRUE,
    viewer.padding = 5
  )



  # create widget
  htmlwidgets::createWidget(
    name = 'fpView',
    x,
    dependencies = deps,
    sizingPolicy = sizing,
    package = 'mapview'
  )

}


#' Widget output function for use in Shiny
#'
#' @export
fpViewOutput <- function(outputId, width = '100%', height = '400px') {
  shinyWidgetOutput(outputId, 'fpView', width, height, package = 'mapview')
}

#' Widget render function for use in Shiny
#'
#' @export
renderfpView <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  shinyRenderWidget(expr, fpViewOutput, env, quoted = TRUE)
}


#' Leaflet maps for big line and polygon data sets
#'
#' @description bView is designed for rendering fairly big line and polyline vector data sets on base of leaflet maps without suffering the typical tough response times of the common leaflet maps. or points it is better to use \code{fpView}. It is utilising rtree, HTML5 canvas.
#'
#'@references  bView is using concepts and code of Sumbera \url{https://gist.github.com/Sumbera/c67e5551b21c68dc8299} and Oscar Hidalgo \url{http://www.cyoarte.com/i+d/cphp/}
#'
#'@note It is somehow important to understand the rendering concept. Due to the huge data sets the data is rendered using the canvas concept of HTML5. This is in questions of rendering speed very effecient but it is not easy to make the data responsive ( future plan is to implement canvas only concepts). For getting informations by click on the features one has  to zoom into the map until the features can be recognized and turn to magenta. At this point you have full access to the vectors. This is provided by using a rtree implementation. Not perfect but it works fully on the client side of the browser. The most time consuming part is the conversion of the data to the geojson format.
#'
#' @param x  \code{\link{sp}} an object one of : SpatialPointDataframe,SpatialPolygonsDataFrame,SpatialPolygons, SpatialLinesDataFrame, SpatialLines
#' @param color colors as:  (green,red,blue,teal,yellow,random) for the points/polygons/lines
#' @param x a \code{\link{sp}} object
#' @param color color (palette) of the points/polygons/lines/pixels
#' @param map.types character spcifications for the base maps.
#' see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
#' for available options.
#' @param layer.opacity opacity of the raster layer(s) (not implemented yet)
#' @param legend should a legend be plotted (not implemented yet)
#' @param legend.opacity opacity of the legend (not implemented yet)
#' @param ... additional arguments passed on to repective functions.
#' See \code{\link{addRasterImage}}, \code{\link{addCircles}},
#' \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
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

#' @export bView
#' @docType bView
#' @name bView
#' @rdname bView
#'
#'
### bview -
bView <- function(x,
                  zcol = NULL,
                  map = NULL,
                  burst = FALSE,
                  color = mapViewPalette(7),
                  na.color = mapviewGetOption("nacolor"),
                  values = NULL,
                  map.types = mapviewGetOption("basemaps"),
                  legend = FALSE,
                  layer.opacity = 0.4,
                  legend.opacity = 1,
                  weight = 2,
                  verbose = mapviewGetOption("verbose"),
                  layer.name = deparse(substitute(x,
                                                  env = parent.frame())),
                  popup = NULL,
                  ...) {
  # check if a sp object exist
  library("gdalUtils")
  library("rgdal")
  data<- x


  ## temp dir
  tmpPath <- makepath()[[1]][1]
  pathJsonFn <- makepath()[[2]][1]
  jsonFn <- makepath()[[3]][1]

  if (!is.null(data)) {
    # first we have  to deproject any data
    data.latlon <- spTransform(data,CRS("+init=epsg:4326"))
    # write to a file to be able to use ogr2ogr
    writeOGR(data.latlon, dsn = tmpPath, layer = "shape", driver="ESRI Shapefile", overwrite_layer = TRUE)
    # convert it to geojson with ogr2ogr
    ogr2ogr(src_datasource_name = paste0(tmpPath,"/shape.shp"), dst_datasource_name = pathJsonFn, f = "GeoJSON", overwrite = TRUE)
    # get rid of tmp file
    file.remove(paste0(tmpPath,"/",dir(path = tmpPath, pattern = "shape")))
    # for fastet json read in a html document we wrap it with var data = {};
    lns <- readLines(pathJsonFn)
    lns[1] <- 'var data = {'
    lns[length(lns)]<- '};'
    writeLines(lns, pathJsonFn)
    # estimating the zommlevel and center point of the map
    zoom<- getZoomLevel(data.latlon)
  } else {
    NULL
  }

  # create list of user data that is passed to the widget
  x = list(color <- color,
           layer <- map.types,
           data  <- 'undefined',
           html <- getStyle(),
           centerLat <- zoom[3],
           centerLon <- zoom[2],
           zoom <- zoom[1],
           opacity <- layer.opacity,
           weight <- weight)

  # creating the widget
  bViewInternal(jFn = pathJsonFn,  args = x)

}

  bViewInternal <- function(jFn = NULL, args = NULL) {

  x <- list(args = args)
  data_dir <- dirname(jFn)
  data_file <- basename(jFn)
  dep1 <- htmltools::htmlDependency(name = "data",
                                    version = "1",
                                    src = c(file = data_dir),
                                    script = list(data_file))
  deps <- list(dep1)

  sizing = htmlwidgets::sizingPolicy(
    browser.fill = TRUE,
    viewer.fill = TRUE,
    viewer.padding = 5
    )
  # create widget
  htmlwidgets::createWidget(
    name = 'bView',
    x,
    dependencies = deps,
    sizingPolicy = sizing,
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

getZoomLevel <- function (x = data.latlon){
  # we need scale and zoom so we approximate the area and zoom factor
  ext <- extent(x)
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
  param = list(zoomlevel,xc,yc)
  return(param)
}

makepath <- function (){
  tmpPath <- tempfile()
  dir.create(tmpPath)
  baseFn <- "data"
  extFn <- "json"
  jsonFn <- paste0(baseFn,".",extFn)
  pathJsonFn <- paste0(tmpPath,"/",jsonFn)
  return(list(tmpPath,pathJsonFn,jsonFn))
}

getStyle <- function(){
  htmlTemplate <- paste(
    "<html>",
    "<head>",
    "<style>",
    "#popup",
    "{font-family: Arial, Helvetica, sans-serif;width: 20%;border-collapse: collapse;}",
    "#popup td {font-size: 1em;border: 0px solid #85ADFF;padding: 3px 20px 3px 3px;}",
    "#popup tr.alt td {color: #000000;background-color: #F0F5FF;}",
    "#popup tr.coord td {color: #000000;background-color: #A8E6A8;}",
    "div.scrollableContainer {max-height: 200px;max-width: 100%;overflow-y: auto;overflow-x: auto;margin: 0px;background: #D1E0FF;}",
    "</style>",
    "</head>",
    "<body>",
    "<div class='scrollableContainer'>",
    "<table class='popup scrollable'>",
    "<table id='popup'>")
  return(htmlTemplate)
}