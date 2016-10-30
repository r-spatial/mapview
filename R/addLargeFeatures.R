addLargeFeatures <- function(map,
                             x,
                             color = "#03F",
                             weight = 4,
                             fillOpacity = 0.4,
                             opacity = 0.9,
                             radius = 8,
                             verbose = mapviewGetOption("verbose"),
                             group = deparse(substitute(x)),
                             ...)
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
    # if (!is.null(zcol)) {
    #   keep <- c(keep, "color")
    #   x@data$color <- color
    #   col <- color[1]
    # } else {
      col <- color[1]
      x@data$color <- color
      keep <- c(keep, "color")
    # }

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

    # estimate the minimum zoomlevel for the rtree part
    # using an empirically (from OSM data) derived function with noFeatures as f(x)
    # scaled by the coarse assumption that a polygons lines and points
    # have an formal relationship of at least 1 to 2 to 3 points each
    # that leads to something like the divisor 1 2 5
    if (class(x)[1] == 'SpatialPolygonsDataFrame'){
      noFeature <- length(x@polygons)
      noF <- noFeature / 1
    } else if (class(x)[1] == 'SpatialLinesDataFrame'){
      noFeature <- length(x@lines)
      noF <- noFeature / 1
    } else {
      noFeature <- nrow(x) #length(x@coords)
      noF <- noFeature / 5
    }

    zoom <- floor(-0.000000000429 * (noF^2) + 0.000148 * noF + 1)
    if (zoom > 14) {zoom <- 14}
    if (zoom < 9) {zoom <- 9}
    # to be done

    # getting the extent and map center
    ext <- raster::extent(x)
    xArea <- (ext@ymax-ext@ymin)*(ext@xmax-ext@xmin)
    yc <- (ext@ymax-ext@ymin) * 0.5  + ext@ymin
    yc <- (ext@ymax-ext@ymin) * 0.5 + ext@ymin
    xc <- (ext@xmax-ext@xmin) * 0.5 + ext@xmin



  }

  # create list of user data that is passed to the widget
  lst_x <- list(color = col,
                #layer = map.types,
                data  = 'undefined',
                html = getPopupStyle(),
                centerLat = yc,
                centerLon = xc,
                opacity = opacity,
                alpharegions = fillOpacity,
                cex = radius,
                weight = weight,
                layername = group,
                xmax = ext@xmax,
                ymax = ext@ymax,
                xmin = ext@xmin,
                ymin = ext@ymin,
                zoom = zoom)

  # creating the widget
  # bViewInternal(jFn = pathJsonFn,  x = lst_x)
  map$dependencies <- c(map$dependencies,
                        largeFeaturesDependencies(),
                        dataDependency(jFn = pathJsonFn))
  leaflet::invokeMethod(map, leaflet:::getMapData(map),
                        'addLargeFeatures', lst_x)

}

largeFeaturesDependencies <- function() {
  list(
    htmltools::htmlDependency(
      "LargeFeatures",
      '0.0.1',
      system.file("htmlwidgets/lib/large", package = "mapview"),
      script = c("addLargeFeatures.js",
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
