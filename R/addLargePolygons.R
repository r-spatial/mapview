addLargePolygons <- function(map,
                             x,
                             zcol = NULL,
                             color = mapviewGetOption("vector.palette"),
                             at = NULL,
                             na.color = mapviewGetOption("na.color"),
                             values,
                             map.types = mapviewGetOption("basemaps"),
                             alpha.regions = 0.9,
                             lwd = 2,
                             verbose = mapviewGetOption("verbose"),
                             layer.name = deparse(substitute(x,
                                                             env = parent.frame())),
                             popup = NULL)
{


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
    color <- mapviewColors(x,
                           zcol = zcol,
                           colors = color,
                           at = at,
                           na.color = na.color)
    x@data$color <- color
    # write to a file to be able to use ogr2ogr
    rgdal::writeOGR(x, paste(tmpPath, "data.geojson", sep=.Platform$file.sep), "OGRGeoJSON", driver="GeoJSON")
    
    # for fastet json read in a html document we wrap it with var data = {};
    #lns<-paste('var data = ', paste(readLines(pathJsonFn), collapse="\n"),';')
    lns <- data.table::fread(pathJsonFn, header = FALSE, sep = "\n",
                             data.table = FALSE)
    lns[1,] <- 'var data = {'
    lns[length(lns[,1]),]<- '};'
    
    write.table(lns, pathJsonFn, sep="\n", row.names=FALSE, col.names=FALSE, quote = FALSE)

    if (class(x)[1] == 'SpatialPolygonsDataFrame'){
      noFeature <- length(x@polygons)
    } else if (class(x)[1] == 'SpatialLinesDataFrame'){
      noFeature <- length(x@lines)
    } else {
      noFeature <- length(x@coords)
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
  # bViewInternal(jFn = pathJsonFn,  x = lst_x)
  map$dependencies <- c(map$dependencies,
                        largePolygonsLinesDependencies(),
                        dataDependency(jFn = pathJsonFn))
  leaflet::invokeMethod(map, leaflet:::getMapData(map),
                        'addLargePolygonsLines', lst_x)

}

largePolygonsLinesDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "LargePolygonsLines",
      '0.0.1',
      system.file("htmlwidgets/lib/large", package = "mapview"),
      script = c("addLargePolygonsLines.js",
                 "leaflet.label.js",
                 "leaflet.ajax.js",
                 "leaflet-providers.js",
                 "geojson-vt-dev.js",
                 "L.CanvasTiles.js",
                 "rtree.min.js",
                 "jquery.min.js"),
      stylesheet = c("leaflet.css",
                     "leafletfix.css",
                     "leaflet.label.css",
                     "gh-fork-ribbon.css",
                     "gh-fork-ribbon.ie.css")
    ))
}


dataDependency <- function(jFn) {
  data_dir <- dirname(jFn)
  data_file <- basename(jFn)
  list(
    htmltools::htmlDependency(
      name = "data",
      version = "1",
      src = c(file = data_dir),
      script = list(data_file)))
}
