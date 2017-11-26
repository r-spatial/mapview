addStarsImage <- function(map,
                          x,
                          bounds,
                          colors = "Spectral",
                          opacity = 1,
                          attribution = NULL,
                          layerId = NULL,
                          group = NULL,
                          project = FALSE,
                          maxBytes = 4*1024*1024) {
  stopifnot(inherits(x, "stars"))

  if (is.null(group)) group = "stars"
  # if (project) {
  #   projected <- projectRasterForLeaflet(x)
  # } else {
  projected <- x
  # }
  # bounds <- raster::extent(raster::projectExtent(raster::projectExtent(x, crs = sp::CRS(epsg3857)), crs = sp::CRS(epsg4326)))

  jsgroup <- gsub(".", "", make.names(group), fixed = TRUE)

  tmp <- makepathStars(as.character(jsgroup))
  pathDatFn <- tmp[[2]][1]
  starspathDatFn <- tmp[[3]][1]
  datFn <- tmp[[4]][1]

  pre <- paste0('var ', jsgroup, ' = ')
  writeLines(pre, pathDatFn)
  cat('[', stars2Array(projected), ']',
      file = pathDatFn, sep = "", append = TRUE)
  # file.append(pathDatFn, starspathDatFn)
  # file.remove(starspathDatFn)

  if (!is.function(colors)) {
    colors <- colorNumeric(colors, domain = NULL,
                           na.color = "#00000000", alpha = TRUE)
  }

  tileData <- as.numeric(projected[[1]]) %>%
    colors() %>% col2rgb(alpha = TRUE) %>% as.raw()
  dim(tileData) <- c(4, nrow(projected), ncol(projected))
  pngData <- png::writePNG(tileData)
  if (length(pngData) > maxBytes) {
    stop("Raster image too large; ",
         length(pngData),
         " bytes is greater than maximum ",
         maxBytes,
         " bytes")
  }
  encoded <- base64enc::base64encode(pngData)
  uri <- paste0("data:image/png;base64,", encoded)

  latlng <- list(
    list(raster::ymax(bounds), raster::xmin(bounds)),
    list(raster::ymin(bounds), raster::xmax(bounds))
  )

  map$dependencies <- c(map$dependencies,
                        starsDataDependency(jFn = pathDatFn,
                                            counter = 1,
                                            group = jsgroup))

  invokeMethod(map, getMapData(map), "addRasterImage", uri, latlng, opacity,
               attribution, layerId, group) %>%
    expandLimits(c(raster::ymin(bounds), raster::ymax(bounds)),
                 c(raster::xmin(bounds), raster::xmax(bounds)))
}


stars2Array = function(x) {
  a = paste(
    sapply(seq(nrow(x[[1]])), function(i) {
      paste0(
        '[', gsub("NA", "null", paste(as.numeric(x[[1]][i, ]), collapse = ",")), ']'
      )
    }),
    collapse = ","
  )
}






makepathStars <- function(group) {
  dirs <- list.dirs(tempdir())
  # tmpPath <- grep(utils::glob2rx("*data_large*"), dirs, value = TRUE)
  # if (length(tmpPath) == 0) {
  tmpPath <- paste(tempfile(pattern = "data_stars"),
                   createFileId(),
                   sep = "_")
  dir.create(tmpPath)
  # }
  baseFn <- paste("data_stars", group, sep = "_")
  extFn <- "txt"
  datFn <- paste0(baseFn, createFileId(), ".", extFn)
  pathDatFn <- paste0(tmpPath, "/", datFn)
  starspathDatFn <- paste0(tmpPath, "/", "stars_", datFn)
  return(list(tmpPath, pathDatFn, starspathDatFn, datFn))
}


starsDataDependency <- function(jFn, counter = 1, group) {
  data_dir <- dirname(jFn)
  data_file <- basename(jFn)
  list(
    htmltools::htmlDependency(
      name = group,
      version = counter,
      src = c(file = data_dir),
      script = list(data_file)))
}


