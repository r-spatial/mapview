if (!isGeneric('xVecView')) {
  setGeneric('xVecView', function(x, ...)
    standardGeneric('xVecView'))

}
# Leaflet maps for big point data
#
# @description fpView is designed for rendering fairly big point vector data sets on base of leaflet maps without suffering the typical tough response times of the common leaflet maps. It utilises HTML5 canvas and webGL technics.
#
# @references  fpView is using a modified and adapted implementation of : \url{https://github.com/robertleeplummerjr/Leaflet.glify}
#
#
# @note It is import to understand that the accurracy of the rendering is about
#  1.1 m at the equator up to 20 cm around 75 degree latitude. You will get an arbitrary result if the
#   accurracy of your points requires more than 5 decimal digits.
#
# @param x  \code{\link{sp}} SpatialPointDataframe object
# @param color colors as:  (green,red,blue,teal,yellow,random) for the points/polygons/lines
# @param x a \code{\link{raster}}* object
# @param color color (palette) of the points/polygons/lines/pixels
# @param map.types character spcifications for the base maps.
# see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
# for available options.
# @param alpha opacity of the raster layer(s) (not implemented yet)
# @param legend should a legend be plotted (not implemented yet)
# @param legend.opacity opacity of the legend (not implemented yet)
# @param ... additional arguments passed on to repective functions.
# See \code{\link{addRasterImage}}, \code{\link{addCircles}},
# \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
# @author
# Chris Reudenbach
#
# @examples
# \dontrun{
# ### we need sp and raster ###
#  library(sp)
#  library(raster)
#  library(ggplot2)

# ### take the meuse data
#  data(meuse)
#  coordinates(meuse) <- ~x+y
#  proj4string(meuse) <- CRS("+init=epsg:28992")
#  meuse <- spTransform(meuse,CRS("+init=epsg:3857"))

#  mapview(meuse)

# ### Now we go  bigger
# ### get the diamonds data
#   big <- diamonds[rep(seq_len(nrow(diamonds)), 50),]
#   big$cut <- as.character(big$cut)
#   big$color <- as.character(big$color)
#   big$clarity <- as.character(big$clarity)

# ### provide some random positions
#   big$x <- rnorm(nrow(big), 10, 3)
#   big$y <- rnorm(nrow(big), 50, 3)
#   coordinates(big) <- ~x+y
#   proj4string(big) <- CRS("+init=epsg:4326")

#   mapview:::fpView(big, color = 'blue')


# ### some benchmarks
# # random point colors is slower
#  system.time(fpView(big))
#
# @keywords internal
#
fpView <- function(x,
                   zcol = NULL,
                   color = mapviewGetOption("vector.palette")(256),
                   na.color = mapviewGetOption("na.color"),
                   values = NULL,
                   map.types = mapviewGetOption("basemaps"),
                   alpha = 0.8,
                   weight = 2,
                   verbose = mapviewGetOption("verbose"),
                   layer.name = deparse(substitute(x,
                                                   env = parent.frame())),
                   popup = NULL,
                   ...) {

  ## temp dir
  ## temp dir
  tmp <- makepath()
  tmpPath <- tmp[[1]][1]
  pathJsonFn <- tmp[[2]][1]
  jsonFn <- tmp[[3]][1]


  # check if a sp object exist
  if (!is.null(x)) {

    # check if  x is a dataframe
    x <- toSPDF(x)

    # check if data has a correct latlong WGS84 proj4 string
    x@proj4string@projargs<-compareProjCode(strsplit(x@proj4string@projargs,split = " "))

    # check projection
    x <- spCheckAdjustProjection(x)

    # get the variable names
    cnames <- colnames(x@data)

    # apply zcol
    if (!is.null(zcol)) {
      cnames <- zcol
    }

    # integrate the coordinates
    x@data$x<-x@coords[,1]
    x@data$y<-x@coords[,2]
    x@data <- x@data[,c("x","y",cnames)]

    # generate reduced geojson string
    data.json <- coords2JSON(as.matrix(x@data))

    # write geojson file to temp dir
    file.create(pathJsonFn)
    fileConn <- file(pathJsonFn)
    write(data.json, fileConn)
    close(fileConn)

    # get extent and center of area
    ext <- extent(x)
    yc <- (ext@ymax-ext@ymin) * 0.5  + ext@ymin
    xc <- (ext@xmax-ext@xmin) * 0.5 + ext@xmin



    # create the popups
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
    lst_x = list(
      color = col2Hex(color),
      layer = map.types,
      data  = "undefined",
      cnames = cnames,
      centerLat = yc,
      centerLon = xc,
      popTemplate = getPopupStyle(),
      cHelp = cHelp,
      layer.opacity = alpha,
      layername = layer.name,
      xmax = ext@xmax,
      ymax = ext@ymax,
      xmin = ext@xmin,
      ymin = ext@ymin,
      values = values
    )
  }

  # now creating the widget
  fpViewInternal(jFn = pathJsonFn,  x = lst_x)
}

### fpViewInternal creates fpView widget =================================================
fpViewInternal <- function(jFn = NULL, x = NULL) {

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

### fpViewOutput Widget output function for use in Shiny =================================================
#
fpViewOutput <- function(outputId, width = '100%', height = '400px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'fpView', width, height, package = 'mapview')
}

### renderfpView Widget render function for use in Shiny =================================================
#
renderfpView <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, fpViewOutput, env, quoted = TRUE)
}


# Leaflet maps for big line and polygon data sets
#
# @description bView is designed for rendering fairly big line and polyline vector data sets on base of leaflet maps without suffering the typical tough response times of the common leaflet maps. or points it is better to use \code{fpView}. It is utilising rtree, HTML5 canvas.
#
#@references  bView is using concepts and code of Sumbera \url{https://gist.github.com/Sumbera/c67e5551b21c68dc8299} and Oscar Hidalgo \url{http://www.cyoarte.com/i+d/cphp/}
#
#@note It is somehow important to understand the rendering concept. Due to the huge data sets the data is rendered using the canvas concept of HTML5. This is in questions of rendering speed very effecient but it is not easy to make the data responsive ( future plan is to implement canvas only concepts). For getting informations by click on the features one has  to zoom into the map until the features can be recognized and turn to magenta. At this point you have full access to the vectors. This is provided by using a rtree implementation. Not perfect but it works fully on the client side of the browser. The most time consuming part is the conversion of the data to the geojson format.
#
# @param x  \code{\link{sp}} an object one of : SpatialPointDataframe,SpatialPolygonsDataFrame,SpatialPolygons, SpatialLinesDataFrame, SpatialLines
# @param color colors as:  (green,red,blue,teal,yellow,random) for the points/polygons/lines
# @param x a \code{\link{sp}} object
# @param color color (palette) of the points/polygons/lines/pixels
# @param map.types character spcifications for the base maps.
# see \url{http://leaflet-extras.github.io/leaflet-providers/preview/}
# for available options.
# @param alpha opacity of the raster layer(s) (not implemented yet)
# @param legend should a legend be plotted (not implemented yet)
# @param legend.opacity opacity of the legend (not implemented yet)
# @param ... additional arguments passed on to repective functions.
# See \code{\link{addRasterImage}}, \code{\link{addCircles}},
# \code{\link{addPolygons}}, \code{\link{addPolylines}} for details
# @author
# Chris Reudenbach
# @examples
# \dontrun{
# ### we need sp, raster, gdalUtils and rgdal ###
#  library(sp)
#  library(raster)
#  library("rgdal")
#  library("gdalUtils")
# ### for downloading and benchmarking
#  library(downloader)
#  library(profvis)

# ## load gadmCHE example data
#  data(gadmCHE)

#  mapview(gadmCHE)

#
# ###  to get more suisse data we use OSM as provided by geofabrik
#  download("http://download.geofabrik.de/europe/switzerland-latest.shp.zip",dest="switzerland.zip", mode = "wb")
#  unzip ("switzerland.zip",exdir = "./")

# ## get information of landuse
#  ogrInfo(".", "landuse")

# ## put it in sp object (doesn't matter what type)
#  landuseCH<- readOGR(".","landuse")

# ## map it with mapview
#  mapview(landuseCH)

# ## get information of waterways
#  ogrInfo(".", "waterways")

# ## read it to a sp object
#  waterwaysCH<- readOGR(".","waterways")

# ## map it with mapview
#  mapview(waterwaysCH)

# ### some benchmarks
#  system.time(mapview(gadmCHE))
#  system.time(mapview(landuseCH))
#  system.time(mapview(waterwaysCH))

#}
#

### bView  function Leaflet maps for big line and polygon data sets =================================================

bView <- function(x,
                  zcol,
                  color,
                  na.color,
                  values,
                  map.types,
                  alpha.regions,
                  lwd,
                  verbose,
                  layer.name,
                  popup) {


  ## temp dir
  tmp <- makepath()
  tmpPath <- tmp[[1]][1]
  pathJsonFn <- tmp[[2]][1]
  jsonFn <- tmp[[3]][1]

  if (!is.null(x)) {
    # check and correct if sp object is of type dataframe
    x <- toSPDF(x)

    # check if a correct WGSS84 proj4 string exist
    x@proj4string@projargs<-compareProjCode(strsplit(x@proj4string@projargs,split = " "))

    # check and transform projection
    x <- spCheckAdjustProjection(x)

    # get the variable names
    keep <- colnames(x@data)

    # apply zcol
    if (!is.null(zcol)) {
      keep <- c(zcol)
    }
    x@data <- x@data[(names(x@data) %in% keep)]

    # write to a file to be able to use ogr2ogr
    rgdal::writeOGR(x, dsn = tmpPath, layer = "shape", driver="ESRI Shapefile", overwrite_layer = TRUE)

    # convert it to geojson with ogr2ogr
    gdalUtils::ogr2ogr(src_datasource_name = paste0(tmpPath,"/shape.shp"), dst_datasource_name = pathJsonFn, f = "GeoJSON")

    # alternative way needs about the same time because it engage the same tools
    #geojsonio::geojson_write(x,file = pathJsonFn, precision = 5)

    # get rid of tmp file
    file.remove(paste0(tmpPath,"/",dir(path = tmpPath, pattern = "shape")))

    # for fastet json read in a html document we wrap it with var data = {};
    lns <- data.table::fread(pathJsonFn, header = FALSE, sep = "\n", data.table = FALSE)
    lns[1,] <- 'var data = {'
    lns[length(lns[,1]),]<- '};'
    write.table(lns, pathJsonFn, sep="\n", row.names=FALSE, col.names=FALSE, quote = FALSE)

    if (class(x)[1] == 'SpatialPolygonsDataFrame'){
      noFeature <- length(x@polygons)
    } else if (class(x)[1] == 'SpatialLinesDataFrame'){
      noFeature <- length(x@lines)
    } else {
      # nrow(coordinates(x)
    }
    # to be done

    # getting the extent and map center
    ext <- extent(x)
    xArea <- (ext@ymax-ext@ymin)*(ext@xmax-ext@xmin)
    yc <- (ext@ymax-ext@ymin) * 0.5  + ext@ymin
    yc <- (ext@ymax-ext@ymin) * 0.5 + ext@ymin
    xc <- (ext@xmax-ext@xmin) * 0.5 + ext@xmin

    tmp <- (noFeature / xArea)
    if (tmp > 15000) {
      zoom <- 14
    } else if (tmp <= 15000 & tmp > 12500){
      zoom<- 13
    } else if (tmp <= 12500 & tmp > 10000){
      zoom<- 12
    }else if (tmp <= 10000 & tmp > 7500){
      zoom<- 11
    } else if (tmp <= 7500 & tmp > 5000 ){
      zoom<- 10
    } else if (tmp <= 5000 & tmp > 2000 ){
      zoom<- 9
    } else if (tmp <= 2000 ){
      zoom<- 8
    }
  } else {
    NULL
  }

  # create list of user data that is passed to the widget
  lst_x <- list(color = col2Hex(color),
                layer = map.types,
                data  = 'undefined',
                html = getPopupStyle(),
                centerLat = yc,
                centerLon = xc,
                opacity = alpha.regions,
                weight = lwd,
                layername = layer.name,
                xmax = ext@xmax,
                ymax = ext@ymax,
                xmin = ext@xmin,
                ymin = ext@ymin,
                zoom = zoom,
                values = x@data)

  # creating the widget
  bViewInternal(jFn = pathJsonFn,  x = lst_x)

}

### bViewInternal creates fpView widget =================================================

bViewInternal <- function(jFn = NULL, x = NULL) {


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

### Widget output function for use in Shiny =================================================
#
bViewOutput <- function(outputId, width = '100%', height = '400px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'bView', width, height, package = 'mapview')
}

### Widget render function for use in Shiny =================================================
#
renderbView <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, bViewOutput, env, quoted = TRUE)
}

### makepath creates temp paths and filenames =================================================
makepath <- function (){
  tmpPath <- tempfile()
  dir.create(tmpPath)
  baseFn <- "data"
  extFn <- "geojson"
  jsonFn <- paste0(baseFn,".",extFn)
  pathJsonFn <- paste0(tmpPath,"/",jsonFn)
  return(list(tmpPath,pathJsonFn,jsonFn))
}

### getPopupStyle creates popup style =================================================
getPopupStyle <- function(){
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