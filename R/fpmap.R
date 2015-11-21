if (!isGeneric('fpmap')) {
  setGeneric('fpmap', function(data,col,width,height,zcol,map,burst,radius,map.types,legend,legend.opacity,verbose,layer.name,popup , ...)
    standardGeneric('fpmap'))
}



#' Fast webGl leaflet maps usable for real big data
#'
#' @description fpmap ist a first prototype to render big vector data on base of leaflet maps. It uses webGL and htmlwidgets.
#'
#' This is a modified and adapted implementation of \link{https://github.com/robertleeplummerjr/Leaflet.glify}
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
#'
#' # take the meuse data
#'  data(meuse)
#'  coordinates(meuse) <- ~x+y
#'  proj4string(meuse) <- CRS("+init=epsg:28992")
#'  meuse <- spTransform(meuse,CRS("+init=epsg:3857"))
#'
#' # map it with mapview
#'  mapview(meuse, zcol = 'cadmium')
#'
#' # map it with fpmap
#'  fpmap(data = meuse,col = "random",zcol = 'cadmium')
#'
#' ### some benchmarks
#'  system.time(mapview(meuse, zcol = 'cadmium'))
#'  system.time(fpmap(data = meuse, col = "random",zcol = 'cadmium'))
#'
#' ### Now we go a bit bigger
#'  library(ggplot2)
#'  library(profvis)
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
#'  fpmap(data = big, col='blue')
#'
#' ### some benchmarks
#'  system.time(mapview(big, color='blue'))
#'  system.time(fpmap(data = big, col = "blue"))
#'
#' ### now getting even bigger
#'  big <- diamonds[rep(seq_len(nrow(diamonds)), 30),]
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
#' # map it NOT with leaflet but with fpmap
#'  fpmap(data = big, col = "random")
#'
#' ### some benchmarks
#' # random point colors is slower
#'  system.time(fpmap(data = big, col = "random"))
#' # than unique colors
#'  system.time(fpmap(data = big, col = "blue"))
#' # profVising it
#'  profvis(fpmap(data = big, col = "blue"))
#'
#' @export
#' @docType fpmap
#' @name fpmap
#' @rdname fpmap
#'
#' @import htmlwidgets
#'
## SpatialPointsDataFrame =================================================



fpmap <- function(data,
                  col = 'blue'  ,
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
    numbs <- sapply(df, is.numeric)
    df.xyz<-df[ , numbs]
    drops <- c("x","y","dist.m")
    df.cols<- df.xyz[,!(names(df.xyz) %in% drops)]
    cnames<- colnames(df.cols)
    if (!is.null(zcol)) {cnames<-zcol}
    df.sort<-df.xyz[,c("x","y",cnames)]
    out.matrix = t(t(df.sort))
    data.json <- coords2JSON(out.matrix)
  } else {
    NULL
  }

  # create list of user data that is passed to the widget
  x = list(color <- col,
           layer <- map.types,
           data  <- data.json,
           cnames <- cnames)

  # create widget
  htmlwidgets::createWidget(
    name = 'fpmap',
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
fpmapOutput <- function(outputId, width = '100%', height = '400px') {
  shinyWidgetOutput(outputId, 'fpmap', width, height, package = 'mapview')
}

#' Widget render function for use in Shiny
#'
#' @export
renderfpmap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  shinyRenderWidget(expr, fpmapOutput, env, quoted = TRUE)
}
