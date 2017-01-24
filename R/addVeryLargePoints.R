addVeryLargePoints <- function(map,
                               data,
                               color = "#0033ff",
                               na.color = "transparent",
                               opacity = 0.8,
                               weight = 2,
                               group = deparse(substitute(data)),
                               popup = NULL,
                               ...) {

  ## temp dir
  ## temp dir
  tmp <- makepathLarge(as.character(group))
  tmpPath <- tmp[[1]][1]
  pathJsonFn <- tmp[[2]][1]
  jsonFn <- tmp[[3]][1]


  # check if a sp object exist
  if (!is.null(data)) {

    # check if  x is a dataframe
    data <- toSPDF(data)

    # check if data has a correct latlong WGS84 proj4 string
    data@proj4string@projargs<-compareProjCode(strsplit(data@proj4string@projargs,split = " "))

    # check projection
    data <- spCheckAdjustProjection(data)

    # create dataframe
    cnames <- colnames(data@data)
    data@data$r <- grDevices::col2rgb(color)[1,]
    data@data$g <- grDevices::col2rgb(color)[2,]
    data@data$b <- grDevices::col2rgb(color)[3,]
    data@data$x <- data@coords[,1]
    data@data$y <- data@coords[,2]
    data@data <- data@data[,c("x","y","r","g","b",cnames)]


    # integrate the coordinates



    # generate reduced geojson string
    # gj <- paste('var data = ', geojsonio::geojson_json(x), ';', sep = "\n")
    # writeLines(gj, con = pathJsonFn)
    #data.json <- paste('var data = {[', coords2JSON(as.matrix(x@data)), ']};', sep = "\n")
    data.json <- coords2JSON(as.matrix(data@data))
    # write geojson file to temp dir
    file.create(pathJsonFn)
    fileConn <- file(pathJsonFn)
    write(data.json, fileConn)
    close(fileConn)

    # get extent and center of area
    ext <- raster::extent(data)
    yc <- (ext@ymax-ext@ymin) * 0.5  + ext@ymin
    xc <- (ext@xmax-ext@xmin) * 0.5 + ext@xmin



    # create the popups
    cHelp <- list()
    cHelp[1] <- "<tr class='coord'><td><b>Longitude<b></td><td>"
    cHelp[2] <- "<tr class='coord'><td><b>Latitude<b></td><td>"
    for (i in 1:length(cnames)) {
      if (i %% 2 == 1) {
        cHelp[i + 2] <- paste0("<tr><td>", "<b>", cnames[i], "<b>", "</td><td>")
      } else {
        cHelp[i + 2] <- paste0("<tr class='alt'><td>", "<b>", cnames[i], "<b>", "</td><td>")
      }
    }

    #color <- mapviewColors(x, colors = color, at = at, na.color = na.color)

    # create list of user data that is passed to the widget
    lst_x = list(
      color = "undefined",  # color, #col2Hex(color),
      data  = "undefined",
      #cnames = cnames,
      centerLat = yc,
      centerLon = xc,
      popTemplate = getPopupStyle(),
      cHelp = cHelp,
      layer.opacity = opacity,
      layername = as.character(group),
      xmax = ext@xmax,
      ymax = ext@ymax,
      xmin = ext@xmin,
      ymin = ext@ymin
    )
  }

  # now creating the widget
  # fpViewInternal(jFn = pathJsonFn,  x = lst_x)
  map$dependencies <- c(map$dependencies,
                        veryLargePointsDependencies(),
                        vertexShaderDependency(),
                        fragmentShaderDependency(),
                        veryLargeDataDependency(jFn = pathJsonFn,
                                                group = group))
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

veryLargeDataDependency <- function(jFn, group) {

  data_dir <- dirname(jFn)
  data_file <- basename(jFn)
  list(
    htmltools::htmlDependency(
      name = group,
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
