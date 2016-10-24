addVeryLargePoints <- function(map,
                               x,
                               zcol = NULL,
                               color = mapviewGetOption("vector.palette")(256),
                               at = NULL,
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

    color <- mapviewColors(x,
                           zcol = zcol,
                           colors = color,
                           at = at,
                           na.color = na.color)




    # apply zcol
    if (!is.null(zcol)) {
      x@data$r<-col2rgb(color)[1,]
      x@data$g<-col2rgb(color)[2,]
      x@data$b<-col2rgb(color)[3,]
      x@data$x<-x@coords[,1]
      x@data$y<-x@coords[,2]
      # get the variable names
      cnames <- c("r","g","b",zcol)
      x@data<-x@data[,c("x","y",cnames)]
    }
    else{
      cnames <- colnames(x@data)
      x@data$x<-x@coords[,1]
      x@data$y<-x@coords[,2]
      x@data<-x@data[,c("x","y",cnames) ]

    }

    # integrate the coordinates



    # generate reduced geojson string
    # gj <- paste('var data = ', geojsonio::geojson_json(x), ';', sep = "\n")
    # writeLines(gj, con = pathJsonFn)
    #data.json <- paste('var data = {[', coords2JSON(as.matrix(x@data)), ']};', sep = "\n")
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

    #color <- mapviewColors(x, colors = color, at = at, na.color = na.color)

    # create list of user data that is passed to the widget
    lst_x = list(
      color = color, #col2Hex(color),
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
  # fpViewInternal(jFn = pathJsonFn,  x = lst_x)
  map$dependencies <- c(map$dependencies,
                        veryLargePointsDependencies(),
                        vertexShaderDependency(),
                        fragmentShaderDependency(),
                        veryLargeDataDependency(jFn = pathJsonFn))
  leaflet::invokeMethod(map, leaflet:::getMapData(map),
                        'addVeryLargePoints', lst_x)

}

veryLargePointsDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "VeryLargePoints",
      '0.0.1',
      system.file("htmlwidgets/lib/veryLarge", package = "mapview"),
      script = c("addVeryLargePoints.js",
                 "gl.js",
                 "jquery.min.js",
                 "leaflet.label.js",
                 "leaflet.ajax.js",
                 "leaflet.canvasoverlay.js",
                 "leaflet.glify.js")
    ))
}

veryLargeDataDependency <- function(jFn) {

  data_dir <- dirname(jFn)
  data_file <- basename(jFn)

  list(
    htmltools::htmlDependency(
      name = "data",
      version = "1",
      src = c(file = data_dir),
      attachment = data_file))
}

# dataDependency <- function(jFn) {
#   data_dir <- dirname(jFn)
#   data_file <- basename(jFn)
#   list(
#     htmltools::htmlDependency(
#       name = "data",
#       version = "1",
#       # src = system.file("htmlwidgets/lib/veryLarge", package = "mapview"),
#       # attachment = c(data_file)))
#       src = c(file = data_dir),
#       script = list(data_file)))
# }



vertexShaderDependency <- function() {
  list(
    htmltools::htmlDependency(
      name = "vertex-shader",
      version = "1",
      system.file("htmlwidgets/lib/veryLarge", package = "mapview"),
      attachment = "vertex-shader.glsl"))
}

fragmentShaderDependency <- function() {
  list(
    htmltools::htmlDependency(
      name = "fragment-shader",
      version = "1",
      system.file("htmlwidgets/lib/veryLarge", package = "mapview"),
      attachment = "fragment-shader.glsl"))
}
