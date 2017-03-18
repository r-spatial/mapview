#' Add moderately large datasets with up to ~100k features to a map.
#'
#' @description
#' This function allows users to add moderately sized datasets to a leaflet
#' or mapview map. Things are drawn on a html canvas for performance. Feature
#' querying is supported but only at higher zoom levels to preserve performance.
#'
#' @param map a mapview or leaflet object.
#' @param data the data to be added to the map.
#' @param color color of the features. This can be a single character value,
#' a vector of character values or a function that takes argument \code{n} to
#' create a vector of \code{n} colors.
#' @param weight the weight of the lines.
#' @param radius the radius of the circleMarkers (ignored for lines/polygons).
#' @param opacity the opacity of the stroke paths.
#' @param fillOpacity opacity of the fill (for circleMarkers and polygons).
#' @param canvasOpacity the opacity of features when rendered on canvas.
#' @param group the name of the group the data layer should belong to.
#' THIS NEEDS TO BE A VALID CHARACTER STRING, I.E. CANNOT BE 'NA' OR 'NULL'!
#' @param maxpoints see \code{\link{mapview}} for details.
#' @param attributes an optional attribute table (data.frame) to be used for
#' popups. If NULL (the default) popups will show only the feature ID.
#' @param ... currently not used.
#'
#' @examples
#' \dontrun{
#' library(mapview)
#' library(sp)
#' library(sf)
#' library(ggmap)
#'
#' data(crime)
#' crime <- crime[complete.cases(crime), ]
#'
#' ## sp version
#' coordinates(crime) <- ~ lon + lat
#' proj4string(crime) <- "+init=epsg:4326"
#'
#' leaflet() %>%
#' addProviderTiles("CartoDB.Positron") %>%
#'   addLargeFeatures(crime, group = "crime") %>%
#'   addMouseCoordinates() %>%
#'   addLayersControl(overlayGroups = "crime", position = "topleft")
#'
#' ## sf version
#' crime_sf <- st_as_sf(crime, coords = c("lon", "lat"), crs = "+init=epsg:4326")
#'
#' leaflet() %>%
#' addProviderTiles("CartoDB.Positron") %>%
#'   addLargeFeatures(crime_sf, group = "crime", color = "black") %>%
#'   addMouseCoordinates() %>%
#'   addLayersControl(overlayGroups = c("crime", "crime2"), position = "topleft") %>%
#'   addLargeFeatures(crime_sf, group = "crime2")
#'
#' ### or use mapview which uses this by default for large features
#' mapview(crime, zcol = "offense", cex = 3)
#'
#' }
#'
#'
#' @export addLargeFeatures
#' @name addLargeFeatures
#' @rdname addLargeFeatures
#' @aliases addLargeFeatures
#'
addLargeFeatures <- function(map,
                             data,
                             color = "#03F",
                             weight = 1,
                             radius = 8,
                             opacity = 1,
                             fillOpacity = 0.6,
                             canvasOpacity = 0.5,
                             group = deparse(substitute(data)),
                             maxpoints = getMaxFeatures(data),
                             attributes = NULL,
                             ...) {

  jsgroup <- gsub(".", "", make.names(group), fixed = TRUE)
  ## temp dir
  tmp <- makepathLarge(as.character(jsgroup))
  pathJsonFn <- tmp[[2]][1]
  sfpathJsonFn <- tmp[[3]][1]
  jsonFn <- tmp[[4]][1]

  cntr <- 1

  if (inherits(data, "Spatial")) data <- sf::st_as_sf(data)

  # calculate the maximum number of features to be rendered using RTree
  # based on the mean number of vertices per feature
  maxFeatures <- maxpoints / (npts(data) / length(sf::st_geometry(data)))

  # check and correct if sp object is of type dataframe
  #data <- toSPDF(data)

  # check if a correct WGSS84 proj4 string exist
  #data@proj4string@projargs<-compareProjCode(strsplit(data@proj4string@projargs,split = " "))

  # check and transform projection
  #data <- spCheckAdjustProjection(data)

  # get the variable names
  #geompos <- which(names(data) == "geometry")
  if (inherits(data, "sfc")) {
    if (!is.null(attributes)) {
      d <- cbind(sf2DataFrame(data, remove_sf_column = TRUE), attributes)
    } else {
      d <- sf2DataFrame(data, remove_sf_column = TRUE)
    }
    d$geom <- data
    data <- sf::st_as_sf(d)
  }

  keep <- colnames(data)#[-geompos]

  # apply zcol
  # if (!is.null(zcol)) {
  #   keep <- c(keep, "color")
  #   data@data$color <- color
  #   col <- color[1]
  # } else {
  col <- color[1]
  data$color <- color
  data$"Feature ID" <- getFeatureIds(data)
  # if (inherits(data, "sf")) {
  keep <- unique(c("Feature ID", keep, "color"))
  # } else {
  #   keep <- c(keep, "color")
  # }
  # # }

  data <- data[, keep]

  # write to a file to be able to use ogr2ogr
  # fl <- pathJsonFn #paste(tmpPath, "data.geojson", sep = .Platform$file.sep)
  # rgdal::writeOGR(obj = data, dsn = fl, layer = "OGRGeoJSON", driver = "GeoJSON",
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

  ### geojsonio currently does not support sf, therefore a workaround with st_write ###
  pre <- paste0('var ', jsgroup, ' = ')
  writeLines(pre, pathJsonFn)
  # gj <- paste(pre, geojsonio::geojson_json(data), ';', sep = "\n")
  sf::st_write(data, dsn = sfpathJsonFn, quiet = TRUE)
  file.append(pathJsonFn, sfpathJsonFn)
  file.remove(sfpathJsonFn)
  # gj <- paste(pre, paste(readLines(pathJsonFn), collapse = ""), sep = "")
  # writeLines(gj, con = pathJsonFn)
  # gdalUtils::ogr2ogr(src_datasource_name = sfpathJsonFn,
  #                    dst_datasource_name = pathJsonFn,
  #                    f = "GeoJSON",
  #                    append = TRUE,
  #                    overwrite = TRUE)

  # estimate the minimum zoomlevel for the rtree part
  # using an empirically (from OSM data) derived function with noFeatures as f(data)
  # scaled by the coarse assumption that a polygons lines and points
  # have an formal relationship of at least 1 to 2 to 3 points each
  # that leads to something like the divisor 1 2 5

  # to be done

  # getting the extent and map center
  ext <- createExtent(data)
  # xArea <- (ext@ymax-ext@ymin)*(ext@xmax-ext@xmin)
  yc <- (ext@ymax-ext@ymin) * 0.5 + ext@ymin
  xc <- (ext@xmax-ext@xmin) * 0.5 + ext@xmin

  # create list of user data that is passed to the widget
  lst_x <- list(color = col,
                #layer = map.types,
                data  = 'undefined',
                group = jsgroup,
                html = getPopupStyle(),
                centerLat = yc,
                centerLon = xc,
                opacity = opacity,
                alpharegions = fillOpacity,
                canvasOpacity = canvasOpacity,
                cex = radius,
                weight = weight,
                layername = as.character(group),
                xmax = ext@xmax,
                ymax = ext@ymax,
                xmin = ext@xmin,
                ymin = ext@ymin,
                maxFeatures = as.integer(maxFeatures))

  # creating the widget
  # bViewInternal(jFn = pathJsonFn,  x = lst_x)
  map$dependencies <- c(map$dependencies,
                        largeFeaturesDependencies(),
                        largeDataDependency(jFn = pathJsonFn,
                                            counter = cntr,
                                            group = jsgroup))
  leaflet::invokeMethod(map, leaflet::getMapData(map),
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


largeDataDependency <- function(jFn, counter, group) {
  data_dir <- dirname(jFn)
  data_file <- basename(jFn)
  list(
    htmltools::htmlDependency(
      name = group,
      version = counter,
      src = c(file = data_dir),
      script = list(data_file)))
}
