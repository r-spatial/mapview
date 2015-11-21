if (!isGeneric('fwmap')) {
  setGeneric('fwmap', function(data , extjson ,color  ,width , height, ...)
    standardGeneric('fwmap'))
}

#' Generates an htmlwidget for a fast webGl leaflet map usable for real big data
#'
#' @description fwmap ist a first prototype to render big vector data on base of a leaflet map. It uses webGL and htmlwidgets.
#'
#' This is a modified and adapted implementation of https://github.com/robertleeplummerjr/Leaflet.glify
#'
#' @param data a \code{\link{sp}} SpatialPointDataframe object (currently only)
#' @param extjson path and filename of an external json file providing coordinats in lon lat as: [[11.5922235,46.5435654],[11.5986949,46.5361959]]
#' @param color colors as:  (green,red,blue,teal,yellow,random) for the points/polygons/lines
#' @param width	a valid CSS width
#' @param height	a valid CSS width
#' @param burst whether to show all (TRUE) or only one (FALSE) layers
#' @param zcol attribute name(s) or column number(s) in attribute table
#' of the column(s) to be rendered
#' @param radius attribute name(s) or column number(s) in attribute table
#' of the column(s) to be used for defining the size of circles
#'
#' @author
#' Chris Reudenbach
#' @examples
#' ### raster data ###
#' library(sp)
#' library(raster)
#'
#'  data(meuse)
#'  coordinates(meuse) <- ~x+y
#'  proj4string(meuse) <- CRS("+init=epsg:28992")
#'
#'  fwmap(data=meuse,color = "random")
#'
#'  ## Now we try to get a bigger amount of online data
#'library(ggplot2)
#'library(sp)
#'library(rgdal)
#'library(mapview)
#'library(profvis)

#'## profvis of 50000 rows
#'big <- diamonds[rep(seq_len(nrow(diamonds)), 1),]
#'big$cut <- as.character(big$cut)
#'big$color <- as.character(big$color)
#'big$clarity <- as.character(big$clarity)
#'
#'big$x <- rnorm(nrow(big), 10, 3)
#'big$y <- rnorm(nrow(big), 30, 3)
#'
#'coordinates(big) <- ~x+y
#'proj4string(big) <- CRS("+init=epsg:4326")
#'
#' # do it with pure leaflet
#'leaflet(big) %>% addTiles() %>% addCircleMarkers(popup = big@data$cut)
#'
#'# do it with fastmap
#'fwmap(big)
#'
#'## now get bigger
#'big <- diamonds[rep(seq_len(nrow(diamonds)), 30),]
#'big$cut <- as.character(big$cut)
#'big$color <- as.character(big$color)
#'big$clarity <- as.character(big$clarity)
#'
#'big$x <- rnorm(nrow(big), 10, 3)
#'big$y <- rnorm(nrow(big), 50, 3)
#'
#'coordinates(big) <- ~x+y
#'proj4string(big) <- CRS("+init=epsg:4326")
#'
#'# map it now
#'fwmap(extdata = big, color = "random")
#'
#'# BENCHMARK
#'system.time(fwmap(extdata = big, color = "random"))
#'
#'profvis(fwmap(extdata = big, color = "random"))




#'
#' @export
#' @docType fwmap
#' @name fwmap
#' @rdname fwmap
#'
#' @import htmlwidgets
#'
## SpatialPointsDataFrame =================================================



fwmap <- function(exdata = NULL,
                  extjson = NULL,
                  color = 'blue'  ,
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
  # up to know we need to access local data sources passing htmlwidget
  # https://github.com/ramnathv/htmlwidgets/issues/71
  # https://github.com/ramnathv/htmlwidgets/issues/141
  # The data is copied to the current temporary directory
  # Workflow
  # A) usecase SpatialPointObject:
  # 1) de-project it to epsg:4326 (necessary for the opengl rendering )
  # 2) convert it to a basic json format
  # 3) write it to the temp/library path
  # 4) read it by the browser (start chrome with:   --allow-file-access-from-files )
  # B) Existing json file in epsg:4326 and correct format can be used by applying step 4 & 5

  libpath <- .libPaths()
  dataToLibPath <-
    paste0(libpath[1],"/mapview/htmlwidgets/lib/data")

  if (!is.null(exdata)) {
    data.latlon <- spTransform(exdata,CRS("+init=epsg:4326"))
    df <- as.data.frame(data.latlon)
    df.xyz <- df[c('x','y','depth')]
    out.matrix = t(t(df.xyz))

    #   microbenchmark(
    #     data.json.old<-jsonlite::toJSON(out.matrix),
    #     data.json <- coords2JSON(out.matrix),
    #     times = 10L
    #   )
    data.json <- coords2JSON(out.matrix)

    file.create("data.json")
    fileConn <- file("data.json")
    write(data.json, fileConn)
    close(fileConn)

    file.copy("data.json",dataToLibPath,overwrite = TRUE)
    file.remove("data.json")

  } else {
    if (!is.null(extjson))
    {
      file.copy(extjson,dataToLibPath
                ,recursive = TRUE)
      file.rename(paste0(dataToLibPath,'/',basename(extjson)) , paste0(dataToLibPath,"/data.json"))
    }
  }


  # create list of user data that is passed to the widget


  x = list(color <- color,
           layer <- map.types,
           data  <- data.json)


  # create widget
  htmlwidgets::createWidget(
    name = 'fwmap',
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
fwmapOutput <- function(outputId, width = '100%', height = '400px') {
  shinyWidgetOutput(outputId, 'fwmap', width, height, package = 'mapview')
}

#' Widget render function for use in Shiny
#'
#' @export
renderFwmap <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  shinyRenderWidget(expr, fwmapOutput, env, quoted = TRUE)
}
