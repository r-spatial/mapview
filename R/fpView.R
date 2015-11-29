if (!isGeneric('fpView')) {
  setGeneric('fpView', function(data,col,width,height,zcol,map,burst,radius,map.types,legend,legend.opacity,verbose,layer.name,popup , ...)
    standardGeneric('fpView'))
}

#' Leaflet maps for big data
#'
#' @description fpView is a first prototype for rendering big data (points) on base of leaflet maps utilizing webGL and htmlwidgets.
#'
#' This is a modified and adapted implementation of \url{https://github.com/robertleeplummerjr/Leaflet.glify}
#'
#'@note It is import to understand that the accurracy of the rendering is about
#'  1.1 m at the equator up to 20 cm around 75°. You will get an arbitrary result if the
#'   accurracy of your points requires more than 5 decimal digits.
#'
#' @param data a \code{\link{sp}} SpatialPointDataframe object (currently only)
#' @param color colors as:  (green,red,blue,teal,yellow,random) for the points/polygons/lines
#' @param width	a valid CSS width
#' @param height	a valid CSS width
#' @param burst whether to show all (TRUE) or only one (FALSE) layers
#' @param zcol attribute name(s) or column number(s) in attribute table
#' of the column(s) to be rendered (up to now only numeric is supported)
#' @param radius attribute name(s) or column number(s) in attribute table
#' of the column(s) to be used for defining the size of circles
#'
#' @author
#' Chris Reudenbach
#' @examples
#'
#' ### we need sp and raster ###
#'  library(sp)
#'  library(raster)
#'  library(ggplot2)
#'  library(profvis)
#'
#' # take the meuse data
#'  data(meuse)
#'  coordinates(meuse) <- ~x+y
#'  proj4string(meuse) <- CRS("+init=epsg:28992")
#'  meuse <- spTransform(meuse,CRS("+init=epsg:3857"))
#'
#' # map it with mapview
#'  mapview(meuse)
#'
#' # map it with fpView
#'  fpView(data = meuse,col = "random")
#'
#' ### some benchmarks
#'  system.time(mapview(meuse))
#'  system.time(fpView(data = meuse, col = "random"))
#'
#' ### Now we go a bit bigger
#'
#' # get the diamonds data
#'  big <- diamonds[rep(seq_len(nrow(diamonds)), 1),]
#'  big$cut <- as.character(big$cut)
#'  big$color <- as.character(big$color)
#'  big$clarity <- as.character(big$clarity)
#' # provide some random positions
#'  big$x <- rnorm(nrow(big), 10, 3)
#'  big$y <- rnorm(nrow(big), 50, 3)
#'
#'  coordinates(big) <- ~x+y
#'  proj4string(big) <- CRS("+init=epsg:4326")
#'
#' # map it with pure mapview
#'  mapview(big, color='blue')
#'
#' # map it with fastmap
#'  fpView(data = big, col='blue')
#'
#' ### some benchmarks
#'  system.time(mapview(big, color='blue'))
#'  system.time(fpView(data = big, col = "blue"))
#'
#' ### up to about 5 mio points
#'  big <- diamonds[rep(seq_len(nrow(diamonds)), 94),]
#'  big$cut <- as.character(big$cut)
#'  big$color <- as.character(big$color)
#'  big$clarity <- as.character(big$clarity)
#'
#'  big$x <- rnorm(nrow(big), 10, 3)
#'  big$y <- rnorm(nrow(big), 50, 3)
#'
#'  coordinates(big) <- ~x+y
#'  proj4string(big) <- CRS("+init=epsg:4326")
#'
#' # map it NOT with leaflet but with fpView
#'  fpView(data = big, col = "blue")
#'
#' ### some benchmarks
#' # random point colors is slower
#'  system.time(fpView(data = big, col = "random"))
#' # than unique colors
#'  system.time(fpView(data = big, col = "blue"))
#' # profVising it
#'  profvis(fpView(data = big, col = "blue"))
#'
#' @export
#' @docType fpView
#' @name fpView
#' @rdname fpView
#'
#'
## SpatialPointsDataFrame =================================================



fpView <- function(data,
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
    # we need scale and zoom so we approximate the area and zoom factor
    ext <- extent(df.sort)
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
      res <- 156543.03  * cos(yc) / (2 ^ zoomlevel)
      #calculating screen scale assuming screen 96 dpi in/m 1000/25.4
      scale = (96 * 39.37 * res)
      if(scale < lonextent || scale < 75000){
        break
      }
      zoomlevel <- zoomlevel + 1
    }
  } else {
    NULL
  }

  if (nrow(df.sort) > 1.5E06) {
    libpath<- .libPaths()
    dataToLibPath<- paste0(libpath[1],"/mapview/htmlwidgets/lib/data")
    file.create("data.json")
    fileConn <- file("data.json")
    write(data.json, fileConn)
    close(fileConn)
    file.copy("data.json",dataToLibPath,overwrite = TRUE)
    file.remove("data.json")
    data.json <- 'undefined'
  }



  # create list of user data that is passed to the widget
  x = list(color <- col,
           layer <- map.types,
           data  <- data.json,
           cnames <- cnames,
           centerLat <- yc,
           centerLon <- xc,
           zoom <- zoomlevel)

  # create widget
  htmlwidgets::createWidget(
    name = 'fpView',
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
