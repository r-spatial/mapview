addStarsImage <- function(map,
                          x,
                          colors = "Spectral",
                          opacity = 1,
                          attribution = NULL,
                          layerId = NULL,
                          group = NULL,
                          project = FALSE,
                          maxBytes = 4*1024*1024) {
  stopifnot(inherits(x, "stars"))

  if (is.null(group)) group = "stars"
  if (is.null(layerId)) layerId = group
  if (project) {
    projected <- st_transform(x, crs = 4326)
  } else {
    projected <- x
  }
  # bounds <- raster::extent(raster::projectExtent(raster::projectExtent(x, crs = sp::CRS(epsg3857)), crs = sp::CRS(epsg4326)))
  bounds = as.numeric(st_bbox(st_transform(st_as_sfc(st_bbox(st_transform(projected, 3857))), 4326)))

  if (!is.function(colors)) {
    colors <- colorNumeric(colors, domain = NULL,
                           na.color = "#00000000", alpha = TRUE)
  }

  tileData <- as.numeric(projected[[1]][, , 1]) %>%
    colors() %>% col2rgb(alpha = TRUE) %>% as.raw()
  dim(tileData) <- c(4, as.numeric(nrow(projected)), as.numeric(ncol(projected)))
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
    list(bounds[4], bounds[1]),
    list(bounds[2], bounds[3])
  )

  # map$dependencies <- c(map$dependencies,
  #                       starsDataDependency(jFn = pathDatFn,
  #                                           counter = 1,
  #                                           group = jsgroup))

  invokeMethod(map, getMapData(map), "addRasterImage", uri, latlng, opacity,
               attribution, layerId, group) %>%
    expandLimits(c(bounds[2], bounds[4]),
                 c(bounds[1], bounds[3]))
}


stars2Array = function(x) {
  paste(
    sapply(seq(nrow(x[[1]])), function(i) {
      paste0(
        '[', gsub("NA", "null",
                  paste(as.numeric(x[[1]][, , 1][i, ]), collapse = ",")), ']'
      )
    }),
    collapse = ","
  )
}


rasterLayer2Array = function(x) {
  x = as.matrix(x)
  paste(
    sapply(seq(ncol(x)), function(i) {
      paste0(
        '[', gsub("NA", "null",
                  paste(as.matrix(x)[, i], collapse = ",")), ']'
      )
    }),
    collapse = ","
  )
}


image2Array = function(x) {
  switch(class(x)[1],
         "stars" = stars2Array(x),
         "RasterLayer" = rasterLayer2Array(x),
         stop("can only query single raster or stars layers so far"))
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



addImageQuery = function(map,
                         x,
                         group = NULL,
                         layerId = NULL,
                         project = FALSE,
                         type = c("mousemove", "click"),
                         digits,
                         position = 'bottomleft',
                         ...) {

  type = match.arg(type)
  if (missing(digits)) digits = "null"
  if (is.null(group)) group = "stars"
  if (is.null(layerId)) layerId = group

  jsgroup <- gsub(".", "", make.names(group), fixed = TRUE)

  tmp <- makepathStars(as.character(jsgroup))
  pathDatFn <- tmp[[2]][1]
  starspathDatFn <- tmp[[3]][1]
  datFn <- tmp[[4]][1]

  if (project) {
    projected <- st_transform(x, crs = 4326)
  } else {
    projected <- x
  }

  pre <- paste0('var data = data || {}; data["', layerId, '"] = ')
  writeLines(pre, pathDatFn)
  cat('[', image2Array(projected), '];',
      file = pathDatFn, sep = "", append = TRUE)

  ## check for existing layerpicker control
  ctrlid = getCallEntryFromMap(map, "addControl")
  imctrl = unlist(sapply(ctrlid, function(i) {
    "imageValues" %in% map$x$calls[[i]]$args
  }))
  ctrlid = ctrlid[imctrl]

  if (length(ctrlid) == 0) {
    map = addControl(map, NULL, layerId = 'imageValues', position = position)
  }

  map$dependencies <- c(map$dependencies,
                        starsDataDependency(jFn = pathDatFn,
                                            counter = 1,
                                            group = jsgroup))
  map$dependencies = c(map$dependencies,
                       list(htmltools::htmlDependency(
                         version = "0.0.1",
                         name = "joda",
                         src = system.file("htmlwidgets/lib/joda",
                                           package = "mapview"),
                         script = "joda.js")
                       ))

  map = htmlwidgets::onRender(
    map,
    htmlwidgets::JS(
      paste0(
        'function(el, x, data) {
        var map = this;
        map.on("', type, '", function (e) {
          rasterPicker.pick(e, x, ', digits, ');
        });
      }'
      )
    )
  )

  return(map)
}
