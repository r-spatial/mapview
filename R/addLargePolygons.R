addLargePolygons <- function(map,
                             x,
                             zcol = NULL,
                             color = mapviewGetOption("vector.palette"),
                             at = NULL,
                             na.color = mapviewGetOption("na.color"),
                             values,
                             map.types = mapviewGetOption("basemaps"),
                             alpha.regions = 0.4,
                             alpha = 0.9,
                             lwd = 2,
                             cex = 5,
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

    color <- mapviewColors(x,
                           zcol = zcol,
                           colors = color,
                           at = at,
                           na.color = na.color)

    # get the variable names
    keep <- colnames(x@data)

    # apply zcol
    if (!is.null(zcol)) {
      keep <- c(zcol,"color")
      x@data$color <- color
      col<-color[1]
    }
    else
    {col<-color
    x@data$color <- color
    keep <- c(keep,"color")}
    x@data <- x@data[(names(x@data) %in% keep)]

    # write to a file to be able to use ogr2ogr
    # fl <- pathJsonFn #paste(tmpPath, "data.geojson", sep = .Platform$file.sep)
    # rgdal::writeOGR(obj = x, dsn = fl, layer = "OGRGeoJSON", driver = "GeoJSON",
    #                 check_exists = FALSE)
    #
    # # for fastet json read in a html document we wrap it with var data = {};
    # #lns<-paste('var data = ', paste(readLines(pathJsonFn), collapse="\n"),';')
    # lns <- data.table::fread(pathJsonFn, header = FALSE, sep = "\n",
    #                          data.table = FALSE, fill = TRUE)
    # lns[1,] <- 'var data = {'
    # lns[length(lns[,1]),]<- '};'
    #
    # write.table(lns, pathJsonFn, sep="\n", row.names=FALSE, col.names=FALSE, quote = FALSE)

    gj <- paste('var data = ', geojsonio::geojson_json(x), ';', sep = "\n")
    writeLines(gj, con = pathJsonFn)

    if (class(x)[1] == 'SpatialPolygonsDataFrame'){
      noFeature <- length(x@polygons)
      multi<-7
    } else if (class(x)[1] == 'SpatialLinesDataFrame'){
      noFeature <- length(x@lines)
      multi<-7
    } else {
      noFeature <- length(x@coords)
      multi<-40
      # nrow(coordinates(x)
    }
    # to be done

    # getting the extent and map center
    ext <- extent(x)
    xArea <- (ext@ymax-ext@ymin)*(ext@xmax-ext@xmin)
    yc <- (ext@ymax-ext@ymin) * 0.5  + ext@ymin
    yc <- (ext@ymax-ext@ymin) * 0.5 + ext@ymin
    xc <- (ext@xmax-ext@xmin) * 0.5 + ext@xmin

    # estimate minum zoomlevel for the rtree part
    # it is roughly calculated by the number of feature/km**2
    # multi scales empirically the tresholds for polygons lines and points
    tmp <- (noFeature/multi)
  if ( tmp > ceiling(60000 )) {
    zoom <- 14
  }else if (tmp <= ceiling(60000 ) & tmp > ceiling(40000 )) {
    zoom <- 13
    }else if (tmp <= ceiling(40000 ) & tmp > ceiling(30000 )) {
    zoom <- 12
    }else if (tmp <= ceiling(30000 ) & tmp > ceiling(20000 )) {
      zoom <- 11
    } else if (tmp <= ceiling(20000 ) & tmp > ceiling(15000 )){
      zoom<- 10
    } else if (tmp <= ceiling(15000 ) & tmp > ceiling(12500 )){
      zoom<- 9
    }else if (tmp <= ceiling(12500 ) & tmp > ceiling(10000 )){
      zoom<- 8
    } else if (tmp <= ceiling(10000 ) & tmp > ceiling(7500 ) ){
      zoom<- 7
    } else if (tmp <= ceiling(7500 ) & tmp > ceiling(5000 ) ){
      zoom<- 6
    } else if (tmp <= ceiling(5000 ) & tmp > ceiling(2500 ) ){
      zoom<- 5
    } else if (tmp <= ceiling(2500 ) & tmp > ceiling(1000 ) ){
      zoom<- 4
    } else if (tmp <= ceiling(1000 ) & tmp > ceiling(500 ) ){
      zoom<- 3
    }
  } else {
    zoom<-2
  }

  # create list of user data that is passed to the widget
  lst_x <- list(color = col,
                layer = map.types,
                data  = 'undefined',
                html = getPopupStyle(),
                centerLat = yc,
                centerLon = xc,
                opacity = alpha,
                alpharegions=alpha.regions,
                at = at,
                cex = cex,
                weight = lwd,
                layername = layer.name,
                xmax = ext@xmax,
                ymax = ext@ymax,
                xmin = ext@xmin,
                ymin = ext@ymin,
                zoom = zoom,
                values = NULL)

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
