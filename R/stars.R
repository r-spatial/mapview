#' Add stars layer to a leaflet map
#'
#' @param map a mapview or leaflet object.
#' @param x a stars layer.
#' @param band the band number to be plotted.
#' @param colors the color palette (see colorNumeric) or function to use to
#' color the raster values (hint: if providing a function, set na.color
#' to "#00000000" to make NA areas transparent)
#' @param opacity the base opacity of the raster, expressed from 0 to 1
#' @param attribution the HTML string to show as the attribution for this layer
#' @param layerId the layer id
#' @param group the name of the group this raster image should belong to
#' (see the same parameter under addTiles)
#' @param project if TRUE, automatically project x to the map projection
#' expected by Leaflet (EPSG:3857); if FALSE, it's the caller's responsibility
#' to ensure that x is already projected, and that extent(x) is
#' expressed in WGS84 latitude/longitude coordinates
#' @param method the method used for computing values of the new,
#' projected raster image. "bilinear" (the default) is appropriate for
#' continuous data, "ngb" - nearest neighbor - is appropriate for categorical data.
#' Ignored if project = FALSE. See projectRaster for details.
#' @param maxBytes the maximum number of bytes to allow for the projected image
#' (before base64 encoding); defaults to 4MB.
#'
#' @details
#' This is an adaption of \code{\link{addRasterImage}}. See that documentation
#' for details.
#'
#' @examples
#' \dontrun{
#' library(stars)
#' library(leaflet)
#' tif = system.file("tif/L7_ETMs.tif", package = "stars")
#' x = read_stars(tif)
#' leaflet() %>%
#'   addProviderTiles("OpenStreetMap") %>%
#'   addStarsImage(x, project = TRUE)
#' }
#'
#' @export addStarsImage
addStarsImage <- function(map,
                          x,
                          band = 1,
                          colors = "Spectral",
                          opacity = 1,
                          attribution = NULL,
                          layerId = NULL,
                          group = NULL,
                          project = FALSE,
                          method = c("bilinear", "ngb"),
                          maxBytes = 4 * 1024 * 1024) {
  stopifnot(inherits(x, "stars"))
  if (is.null(group)) group = "stars"
  if (is.null(layerId)) layerId = group
  if (project) {
    projected <- sf::st_transform(x, crs = 3857)
  } else {
    projected <- x
  }
  # bounds <- raster::extent(raster::projectExtent(raster::projectExtent(x, crs = sp::CRS(epsg3857)), crs = sp::CRS(epsg4326)))
  bb = sf::st_as_sfc(sf::st_bbox(projected))
  bounds = as.numeric(sf::st_bbox(sf::st_transform(bb, 4326)))
  if (!is.function(colors)) {
    colors <- leaflet::colorNumeric(colors, domain = NULL,
                                    na.color = "#00000000", alpha = TRUE)
  }
  if(length(dim(projected)) == 2) {
    layer = projected[[1]]
  } else {
    layer = projected[[1]][, , band]
  }
  tileData <- as.numeric(layer) %>%
    colors() %>% grDevices::col2rgb(alpha = TRUE) %>% as.raw()
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
  leaflet::invokeMethod(map, getMapData(map), "addRasterImage", uri, latlng,
                        opacity, attribution, layerId, group) %>%
    leaflet::expandLimits(c(bounds[2], bounds[4]),
                          c(bounds[1], bounds[3]))
}


leaflet_stars = function(x,
                         band,
                         map,
                         maxpixels,
                         col.regions,
                         at,
                         na.color,
                         use.layer.names,
                         values,
                         map.types,
                         alpha.regions,
                         legend,
                         legend.opacity,
                         trim,
                         verbose,
                         layer.name,
                         homebutton,
                         native.crs,
                         method,
                         label,
                         query.type,
                         query.digits,
                         query.position,
                         query.prefix,
                         viewer.suppress,
                         ...) {
  if (inherits(map, "mapview")) map = mapview2leaflet(map)
  if (is.null(layer.name)) layer.name = makeLayerName(x, zcol = band)
  if (native.crs) {
    stop("native.crs display of stars layers is not (yet) supported",
         call. = FALSE)
    # plainView(x,
    #           maxpixels = mapviewGetOption("plainview.maxpixels"),
    #           col.regions = col.regions,
    #           at = at,
    #           na.color = na.color,
    #           legend = legend,
    #           verbose = verbose,
    #           layer.name = layer.name,
    #           gdal = TRUE,
    #           ...)
  } else {
    is.fact = FALSE # raster::is.factor(x)
    # ext = raster::extent(raster::projectExtent(x, crs = llcrs))
    m = initMap(map, map.types, sf::st_crs(x)$proj4string, viewer.suppress = viewer.suppress)
    # x = rasterCheckSize(x, maxpixels = maxpixels)
    # x = starsCheckAdjustProjection(x, method)
    ext = createExtent(sf::st_transform(sf::st_as_sfc(sf::st_bbox(x)), crs = 4326))
    # if (!is.na(raster::projection(x)) & trim) x = trim(x)
    # if (is.fact) x = raster::as.factor(x)
    if(length(dim(x)) == 2) layer = x[[1]] else layer = x[[1]][, , band]
    if (is.null(values)) {
      # if (is.fact) {
      #   at = x@data@attributes[[1]]$ID
      # } else {
      offset = diff(range(as.numeric(layer), na.rm = TRUE)) * 0.05
      top = max(as.numeric(layer), na.rm = TRUE) + offset
      bot = min(as.numeric(layer), na.rm = TRUE) - offset
      values = seq(bot, top, length.out = 10)
      values = round(values, 5)
      # }
    } else {
      values = round(values, 5)
    }
    if (is.fact) {
      pal = leaflet::colorFactor(palette = col.regions,
                                 domain = values,
                                 na.color = na.color)
      # pal2 = pal
    } else {
      pal = rasterColors(col.regions,
                         at = at,
                         na.color = na.color)
      if (length(at) > 11) {
        pal2 = leaflet::colorNumeric(palette = col.regions,
                                     domain = at,
                                     na.color = na.color)
      } else {
        pal2 = leaflet::colorBin(palette = col.regions,
                                 bins = length(at),
                                 domain = at,
                                 na.color = na.color)
      }
    }

    if (use.layer.names) {
      grp = names(x)
      layer.name = names(x)
    } else {
      grp = layer.name
    }
    x <- sf::st_transform(x, crs = 3857)
    ## add layers to base map
    m = addStarsImage(map = m,
                      x = x,
                      band = band,
                      colors = pal,
                      project = FALSE,
                      opacity = alpha.regions,
                      group = grp,
                      layerId = grp,
                      ...)
    if (label)
      m = leafem::addImageQuery(m, x, band = band, group = grp, layerId = grp,
                                type = query.type, digits = query.digits,
                                position = query.position, prefix = query.prefix)
    if (legend) {
      # stop("legend currently not supported for stars layers", call. = FALSE)
      ## add legend
      # m = leaflet::addLegend(map = m,
      #                         pal = pal2,
      #                         opacity = legend.opacity,
      #                         values = at,
      #                         title = grp)
      m = addRasterLegend(x = x,
                          map = m,
                          title = grp,
                          group = grp,
                          at = at,
                          col.regions = col.regions,
                          na.color = na.color)
    }
    m = mapViewLayersControl(map = m,
                             map.types = map.types,
                             names = grp)
    m = leaflet::addScaleBar(map = m, position = "bottomleft")
    m = leafem::addMouseCoordinates(m)
    if (homebutton) m = leafem::addHomeButton(m, ext, layer.name = layer.name)
    out = new('mapview', object = list(x), map = m)
    return(out)
  }
}






stars2Array = function(x, band = 1) {
  if(length(dim(x)) == 2) layer = x[[1]] else layer = x[[1]][, , band]
  paste(
    sapply(seq(nrow(x[[1]])), function(i) {
      paste0(
        '[', gsub("NA", "null",
                  paste(as.numeric(layer[i, ]), collapse = ",")), ']'
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


image2Array = function(x, band = 1) {
  switch(class(x)[1],
         "stars" = stars2Array(x, band = band),
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



# addImageQuery = function(map,
#                          x,
#                          group = NULL,
#                          layerId = NULL,
#                          project = FALSE,
#                          type = c("mousemove", "click"),
#                          digits,
#                          position = 'bottomleft',
#                          ...) {
#
#   type = match.arg(type)
#   if (missing(digits)) digits = "null"
#   if (is.null(group)) group = "stars"
#   if (is.null(layerId)) layerId = group
#
#   jsgroup <- gsub(".", "", make.names(group), fixed = TRUE)
#
#   tmp <- makepathStars(as.character(jsgroup))
#   pathDatFn <- tmp[[2]][1]
#   starspathDatFn <- tmp[[3]][1]
#   datFn <- tmp[[4]][1]
#
#   if (project) {
#     projected <- st_transform(x, crs = 4326)
#   } else {
#     projected <- x
#   }
#
#   pre <- paste0('var data = data || {}; data["', layerId, '"] = ')
#   writeLines(pre, pathDatFn)
#   cat('[', image2Array(projected), '];',
#       file = pathDatFn, sep = "", append = TRUE)
#
#   ## check for existing layerpicker control
#   ctrlid = getCallEntryFromMap(map, "addControl")
#   imctrl = unlist(sapply(ctrlid, function(i) {
#     "imageValues" %in% map$x$calls[[i]]$args
#   }))
#   ctrlid = ctrlid[imctrl]
#
#   if (length(ctrlid) == 0) {
#     map = addControl(map, NULL, layerId = 'imageValues', position = position)
#   }
#
#   map$dependencies <- c(map$dependencies,
#                         starsDataDependency(jFn = pathDatFn,
#                                             counter = 1,
#                                             group = jsgroup))
#   map$dependencies = c(map$dependencies,
#                        list(htmltools::htmlDependency(
#                          version = "0.0.1",
#                          name = "joda",
#                          src = system.file("htmlwidgets/lib/joda",
#                                            package = "mapview"),
#                          script = "joda.js")
#                        ))
#
#   map = htmlwidgets::onRender(
#     map,
#     htmlwidgets::JS(
#       paste0(
#         'function(el, x, data) {
#         var map = this;
#         map.on("', type, '", function (e) {
#           rasterPicker.pick(e, x, ', digits, ');
#         });
#       }'
#       )
#     )
#   )
#
#   return(map)
# }
