#### functions for leaflet based rendering by spatial class

### leaflet w RasterLayer =================================================

leafletRL <- function(x,
                      map,
                      maxpixels,
                      col.regions,
                      at,
                      na.color,
                      use.layer.names,
                      values,
                      map.types,
                      alpha,
                      legend,
                      legend.opacity,
                      trim,
                      verbose,
                      layer.name,
                      ...) {

  pkgs <- c("leaflet", "raster", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  is.fact <- raster::is.factor(x)

  m <- initMap(map, map.types, sp::proj4string(x))
  x <- rasterCheckSize(x, maxpixels = maxpixels)
  x <- rasterCheckAdjustProjection(x)

  if (!is.na(raster::projection(x)) & trim) x <- trim(x)

  if (is.fact) x <- raster::as.factor(x)

  if (is.null(values)) {
    if (is.fact) {
      at <- x@data@attributes[[1]]$ID
    } else {
      offset <- diff(range(x[], na.rm = TRUE)) * 0.05
      top <- max(x[], na.rm = TRUE) + offset
      bot <- min(x[], na.rm = TRUE) - offset
      values <- seq(bot, top, length.out = 10)
      values <- round(values, 5)
    }
  } else {
    values <- round(values, 5)
  }

  if (is.fact) {
    pal <- leaflet::colorFactor(palette = col.regions,
                                domain = values,
                                na.color = na.color)
    pal2 <- pal
  } else {
    pal <- mapviewColors(col.regions,
                         at = at,
                         na.color = na.color)

    if (length(at) > 11) {
      pal2 <- leaflet::colorNumeric(palette = col.regions,
                                    domain = at,
                                    na.color = na.color)
    } else {
      pal2 <- leaflet::colorBin(palette = col.regions,
                                bins = length(at),
                                domain = at,
                                na.color = na.color)
    }

  }

  if (use.layer.names) {
    grp <- names(x)
  } else {
    grp <- layer.name
  }

  ## add layers to base map
  m <- leaflet::addRasterImage(map = m,
                               x = x,
                               colors = pal,
                               project = FALSE,
                               opacity = alpha,
                               group = grp,
                               ...)

  if (legend) {
    ## add legend
    m <- leaflet::addLegend(map = m,
                            pal = pal2,
                            opacity = legend.opacity,
                            values = at,
                            title = grp)
  }

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  out <- new('mapview', object = list(x), map = m)

  return(out)

}



### leaflet w RasterStackBrick ============================================

leafletRSB <- function(x,
                       map,
                       maxpixels,
                       col.regions,
                       at,
                       na.color,
                       values,
                       map.types,
                       legend,
                       legend.opacity,
                       trim,
                       verbose,
                       ...) {

  pkgs <- c("leaflet", "raster", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  m <- initMap(map, map.types, sp::proj4string(x))

  if (nlayers(x) == 1) {
    x <- raster(x, layer = 1)
    m <- mapView(x, map = m, maxpixels = maxpixels, map.types = map.types,
                 use.layer.names = TRUE, at = at, col.regions, ...)
    out <- new('mapview', object = list(x), map = m@map)
  } else {
    m <- mapView(x[[1]], map = m, maxpixels = maxpixels, map.types = map.types,
                 use.layer.names = TRUE, at = at, col.regions, ...)
    for (i in 2:nlayers(x)) {
      m <- mapView(x[[i]], map = m@map, maxpixels = maxpixels, map.types = map.types,
                   use.layer.names = TRUE, at = at, col.regions, ...)
    }

    if (length(getLayerNamesFromMap(m@map)) > 1) {
      m <- leaflet::hideGroup(map = m@map,
                              group = layers2bHidden(m@map))
    }
    out <- new('mapview', object = list(x), map = m)
  }

  return(out)

}



### leaflet w SpatialPixelsDataFrame ======================================

leafletPixelsDF <- function(x,
                            zcol,
                            ...) {

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  if(!is.null(zcol)) x <- x[, zcol]

  stck <- do.call("stack", lapply(seq(ncol(x)), function(i) {
    r <- raster::raster(x[, i])
    if (is.factor(x[, i])) r <- raster::as.factor(r)
    return(r)
  }))

  m <- mapView(stck, ...)

  out <- new('mapview', object = list(x), map = m@map)

  return(out)

}



### leaflet w SpatialPointsDataFrame ======================================

leafletPointsDF <- function(x,
                            zcol,
                            map,
                            burst,
                            color,
                            na.color,
                            cex,
                            lwd,
                            alpha,
                            alpha.regions,
                            map.types,
                            legend,
                            legend.opacity,
                            verbose,
                            layer.name,
                            popup,
                            ...) {

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  rad_vals <- circleRadius(x, cex)
  if(!is.null(zcol)) x <- x[, zcol]
  if(!is.null(zcol)) burst <- TRUE

  pop.null <- is.null(popup)

  x <- spCheckObject(x, verbose = verbose)
  x <- spCheckAdjustProjection(x)

  m <- initMap(map, map.types, sp::proj4string(x))

  if (burst) {
    lst <- lapply(names(x), function(j) x[j])

    vals <- lapply(seq(lst), function(i) lst[[i]]@data[, 1])

    pal_n <- lapply(seq(lst), function(i) {
      if (is.factor(lst[[i]]@data[, 1])) {
        leaflet::colorFactor(color, lst[[i]]@data[, 1],
                             levels = levels(lst[[i]]@data[, 1]))
      } else {
        leaflet::colorNumeric(color, vals[[i]],
                              na.color = na.color)
      }
    })

    for (i in seq(lst)) {

      #x <- lst[[i]]

      if (pop.null) popup <- brewPopupTable(lst[[i]])

      m <- leaflet::addCircleMarkers(m,
                                     lng = coordinates(lst[[i]])[, 1],
                                     lat = coordinates(lst[[i]])[, 2],
                                     group = names(lst[[i]]),
                                     color = pal_n[[i]](vals[[i]]),
                                     #radius = cex,
                                     weight = lwd,
                                     opacity = alpha,
                                     fillOpacity = alpha.regions,
                                     popup = popup,
                                     #data = lst[[i]],
                                     radius = rad_vals,
                                     ...)

      if (legend) {
        m <- leaflet::addLegend(map = m, position = "topright",
                                pal = pal_n[[i]],
                                opacity = 1, values = vals[[i]],
                                title = names(lst[[i]]),
                                layerId = names(lst[[i]]))
      }

    }

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = names(x))

    #     m <- leaflet::addLayersControl(map = m,
    #                                    position = mapviewOptions(
    #                                      console = FALSE)$layerscontrolpos,
    #                                    baseGroups = map.types,
    #                                    overlayGroups = names(x))


    if (length(getLayerNamesFromMap(m)) > 1) {
      m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
    }

  } else {

    grp <- layer.name

    if (pop.null) popup <- brewPopupTable(x)

    m <- leaflet::addCircleMarkers(map = m,
                                   lng = coordinates(x)[, 1],
                                   lat = coordinates(x)[, 2],
                                   group = grp,
                                   color = color[length(color)],
                                   #radius = cex,
                                   weight = lwd,
                                   opacity = alpha,
                                   fillOpacity = alpha.regions,
                                   popup = popup,
                                   #data = x,
                                   radius = rad_vals,
                                   ...)

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = grp)

    #     m <- leaflet::addLayersControl(map = m,
    #                                    position = mapviewOptions(
    #                                      console = FALSE)$layerscontrolpos,
    #                                    baseGroups = map.types,
    #                                    overlayGroups = grp)
  }

  out <- new('mapview', object = list(x), map = m)

  return(out)

}



### leaflet w SpatialPoints ===============================================

leafletPoints <- function(x,
                          map,
                          cex,
                          lwd,
                          alpha,
                          alpha.regions,
                          na.color,
                          map.types,
                          verbose,
                          layer.name,
                          ...) {

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  x <- spCheckAdjustProjection(x)

  m <- initMap(map, map.types, sp::proj4string(x))

  txt <- brewPopupTable(x)

  grp <- layer.name

  m <- leaflet::addCircleMarkers(m,
                                 lng = coordinates(x)[, 1],
                                 lat = coordinates(x)[, 2],
                                 radius = cex,
                                 weight = lwd,
                                 opacity = alpha,
                                 fillOpacity = alpha.regions,
                                 group = grp,
                                 popup = txt,
                                 ...)

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  out <- new('mapview', object = list(x), map = m)

  return(out)

}



### leaflet w SpatialPolygonsDataFrame ====================================

leafletPolygonsDF <- function(x,
                              zcol,
                              map,
                              burst,
                              color,
                              na.color,
                              values,
                              map.types,
                              legend,
                              legend.opacity,
                              lwd,
                              alpha,
                              alpha.regions,
                              verbose,
                              layer.name,
                              popup,
                              ...) {

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  x <- spCheckObject(x, verbose = verbose)

  if(!is.null(zcol)) x <- x[, zcol]
  if(!is.null(zcol)) burst <- TRUE

  pop.null <- is.null(popup)

  x <- spCheckAdjustProjection(x)

  m <- initMap(map, map.types, sp::proj4string(x))

  if (burst) {

    lst <- lapply(names(x), function(j) x[j])

    df_all <- lapply(seq(lst), function(i) {
      dat <- data.frame(lst[[i]], stringsAsFactors = TRUE)
      if (is.character(dat[, 1])) {
        dat[, 1] <- factor(dat[, 1], levels = unique(dat[, 1]))
      }
      return(dat)
    })

    vals <- lapply(seq(lst), function(i) df_all[[i]][, 1])

    pal_n <- lapply(seq(lst), function(i) {
      if (is.factor(df_all[[i]][, 1])) {
        leaflet::colorFactor(color, vals[[i]],
                             levels = levels(vals[[i]]),
                             na.color = na.color)
      } else {
        leaflet::colorNumeric(color, vals[[i]],
                              na.color = na.color)
      }
    })

    for (i in seq(lst)) {

      grp <- names(lst[[i]])

      if (pop.null) popup <- brewPopupTable(lst[[i]])

      clrs <- pal_n[[i]](vals[[i]])
      m <- leaflet::addPolygons(m,
                                weight = lwd,
                                opacity = alpha,
                                fillOpacity = alpha.regions,
                                group = grp,
                                color = clrs,
                                popup = popup,
                                data = lst[[i]],
                                ...)

      m <- leaflet::addLegend(map = m, position = "topright",
                              pal = pal_n[[i]], opacity = 1,
                              values = vals[[i]],
                              title = grp)

    }

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = names(x))


    #     m <- leaflet::addLayersControl(map = m,
    #                                    position = mapviewOptions(
    #                                      console = FALSE)$layerscontrolpos,
    #                                    baseGroups = map.types,
    #                                    overlayGroups = names(x))

    if (length(names(x)) > 1) {
      m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
    }

  } else {

    grp <- layer.name

    if (pop.null) popup <- brewPopupTable(x)

    m <- leaflet::addPolygons(m,
                              weight = lwd,
                              opacity = alpha,
                              fillOpacity = alpha.regions,
                              group = grp,
                              color = color[length(color)],
                              popup = popup,
                              data = x,
                              ...)

    #     m <- leaflet::addLayersControl(map = m,
    #                                    position = mapviewOptions(
    #                                      console = FALSE)$layerscontrolpos,
    #                                    baseGroups = map.types,
    #                                    overlayGroups = grp)

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = grp)
  }

  out <- new('mapview', object = list(x), map = m)

  return(out)

}



### leaflet w SPatialPolygons =============================================

leafletPolygons <- function(x,
                            map,
                            na.color,
                            map.types,
                            lwd,
                            alpha,
                            alpha.regions,
                            verbose,
                            layer.name,
                            ...) {

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  x <- spCheckAdjustProjection(x)

  m <- initMap(map, map.types, sp::proj4string(x))

  grp <- layer.name

  m <- leaflet::addPolygons(m,
                            weight = lwd,
                            group = grp,
                            data = x,
                            opacity = alpha,
                            fillOpacity = alpha.regions,
                            ...)

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  out <- new('mapview', object = list(x), map = m)

  return(out)

}



### leaflet w SpatialLinesDataFrame =======================================

leafletLinesDF <- function(x,
                           zcol,
                           map,
                           burst,
                           color,
                           na.color,
                           values,
                           map.types,
                           lwd,
                           alpha,
                           legend,
                           legend.opacity,
                           verbose,
                           layer.name,
                           popup,
                           ...) {

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  if(!is.null(zcol)) x <- x[, zcol]
  if(!is.null(zcol)) burst <- TRUE

  pop.null <- is.null(popup)

  x <- spCheckObject(x, verbose = verbose)
  x <- spCheckAdjustProjection(x)

  m <- initMap(map, map.types, sp::proj4string(x))

  if (burst) {

    lst <- lapply(names(x), function(j) x[j])

    df_all <- lapply(seq(lst), function(i) {
      dat <- data.frame(lst[[i]], stringsAsFactors = TRUE)
      if (any(class(dat[, 1]) == "POSIXt")) {
        dat[, 1] <- as.character(dat[, 1])
      }
      if (is.character(dat[, 1])) {
        dat[, 1] <- factor(dat[, 1], levels = unique(dat[, 1]))
      }
      return(dat)
    })

    vals <- lapply(seq(lst), function(i) df_all[[i]][, 1])

    pal_n <- lapply(seq(lst), function(i) {
      if (is.factor(df_all[[i]][, 1])) {
        leaflet::colorFactor(color, vals[[i]],
                             levels = levels(vals[[i]]),
                             na.color = na.color)
      } else {
        leaflet::colorNumeric(color, vals[[i]],
                              na.color = na.color)
      }
    })

    for (i in seq(lst)) {

      #x <- lst[[i]]

      df <- as.data.frame(sapply(x@data, as.character),
                          stringsAsFactors = FALSE)

      grp <- names(df)[i]

      if (pop.null) popup <- brewPopupTable(lst[[i]])

      m <- leaflet::addPolylines(m,
                                 group = grp,
                                 color = pal_n[[i]](vals[[i]]),
                                 popup = popup,
                                 data = lst[[i]],
                                 weight = lwd,
                                 opacity = alpha,
                                 ...)

      m <- leaflet::addLegend(map = m, position = "topright",
                              pal = pal_n[[i]], opacity = 1,
                              values = vals[[i]], title = grp)

    }

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = names(x))

    if (length(getLayerNamesFromMap(m)) > 1) {
      m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
    }

  } else {

    df <- as.data.frame(sapply(x@data, as.character),
                        stringsAsFactors = FALSE)

    grp <- layer.name

    if (pop.null) popup <- brewPopupTable(x)

    m <- leaflet::addPolylines(m,
                               group = grp,
                               color = color[length(color)],
                               popup = popup,
                               data = x,
                               weight = lwd,
                               opacity = alpha,
                               ...)

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = grp)
  }

  out <- new('mapview', object = list(x), map = m)

  return(out)

}



### leaflet w SpatialLines ================================================

leafletLines <- function(x,
                         map,
                         na.color,
                         map.types,
                         lwd,
                         alpha,
                         verbose,
                         layer.name,
                         ...) {

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  #llcrs <- CRS("+init=epsg:4326")@projargs

  x <- spCheckAdjustProjection(x)

  m <- initMap(map, map.types, sp::proj4string(x))

  grp <- layer.name

  m <- leaflet::addPolylines(m,
                             group = grp,
                             data = x,
                             weight = lwd,
                             opacity = alpha,
                             ...)

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  out <- new('mapview', object = list(x), map = m)

  return(out)

}



### leaflet w ppp =========================================================

# leafletPPP <- function(x,
#                        map,
#                        na.color,
#                        map.types,
#                        verbose,
#                        layer.name,
#                        ...) {
#
#   pkgs <- c("leaflet", "sp", "magrittr")
#   tst <- sapply(pkgs, "requireNamespace",
#                 quietly = TRUE, USE.NAMES = FALSE)
#
#   marks_exist <- if (x$markformat == "none") FALSE else TRUE
#
#   if (marks_exist) {
#     sp_ppp <- as(x, "SpatialPointsDataFrame")
#     mat <- rbind(x$window$xrange, x$window$yrange)
#     rownames(mat) <- c("x", "y")
#     colnames(mat) <- c("min", "max")
#     sp_ppp@bbox <- mat
#   } else {
#     sp_ppp <- as(x, "SpatialPoints")
#     mat <- rbind(x$window$xrange, x$window$yrange)
#     rownames(mat) <- c("x", "y")
#     colnames(mat) <- c("min", "max")
#     sp_ppp@bbox <- mat
#   }
#
#   sp_ppp <- checkAdjustProjection(sp_ppp)
#   out <- viewExtent(sp_ppp) + sp_ppp
#
#   return(out)
#
# }



### leaflet w missing =====================================================

leafletMissing <- function(map.types,
                           easter.egg) {

  if(easter.egg) {
    envinMR <- data.frame(x = 8.771676,
                          y = 50.814891,
                          envinMR = "envinMR")
    sp::coordinates(envinMR) <- ~x+y
    sp::proj4string(envinMR) <- sp::CRS(llcrs)
    m <- initBaseMaps(map.types)

    pop <- paste("<center>", "<b>", "mapview", "</b>", "<br>", " was created at",
                 "<br>",
                 '<a target="_blank" href="http://environmentalinformatics-marburg.de/">Environmental Informatics Marburg</a>',
                 "<br>", "by ", "<br>",
                 '<a target="_blank" href="http://umweltinformatik-marburg.de/en/staff/tim-appelhans/">Tim Appelhans</a>',
                 "<br>", "and is released under", "<br>",
                 strsplit(utils::packageDescription("mapview", fields = "License"), "\\|")[[1]][1],
                 "<br>", "<br>",
                 '<hr width=50% style="border: none; height: 1px; color: #D8D8D8; background: #D8D8D8;"/>',
                 "<br>",
                 "Please cite as: ", "<br>",
                 attr(unclass(utils::citation("mapview"))[[1]], "textVersion"),
                 "<br>", "<br>",
                 'A BibTeX entry for LaTeX users can be created with',
                 "<br>",
                 '<font face="courier">',
                 'citation("mapview")',
                 '</font face="courier">',
                 "</center>")
    m <- leaflet::addCircles(data = envinMR, map = m,
                             fillColor = "white",
                             color = "black",
                             weight = 6,
                             opacity = 0.8,
                             fillOpacity = 0.5,
                             group = "envinMR",
                             popup = pop)
    m <- leaflet::addPopups(map = m,
                            lng = 8.771676,
                            lat = 50.814891,
                            popup = pop)
    m <- mapViewLayersControl(map = m, map.types = map.types,
                              names = "envinMR")
    m <- leaflet::setView(map = m, 8.771676, 50.814891, zoom = 18)
    out <- new('mapview', object = list(NULL), map = m)
  } else {
    m <- initBaseMaps(map.types)
    m <- leaflet::setView(map = m, 8.770862, 50.814772, zoom = 18)
    m <- leaflet::addLayersControl(map = m, baseGroups = map.types,
                                   position = mapviewOptions(
                                     console = FALSE)$layerscontrolpos)
    out <- new('mapview', object = list(NULL), map = m)
  }
  return(out)

}


# ###########################################################################
# ############################ PLAIN ########################################
# ###########################################################################
#
#
# ### plain w RasterLayer ===================================================
#
# leafletPlainRL <- function(x,
#                            map,
#                            maxpixels,
#                            color,
#                            na.color,
#                            use.layer.names,
#                            values,
#                            alpha,
#                            legend,
#                            legend.opacity,
#                            trim,
#                            verbose,
#                            layer.name,
#                            ...) {
#
#   pkgs <- c("leaflet", "raster", "magrittr")
#   tst <- sapply(pkgs, "requireNamespace",
#                 quietly = TRUE, USE.NAMES = FALSE)
#
#   is.fact <- raster::is.factor(x)
#
#   if (is.null(map)) m <- leaflet::leaflet() else m <- map
#   x <- rasterCheckSize(x, maxpixels = maxpixels)
#
#   if (trim) x <- trim(x)
#
#   if (is.fact) x <- raster::as.factor(x)
#
#   if (is.null(values)) {
#     if (is.fact) {
#       values <- x@data@attributes[[1]]$ID
#     } else {
#       offset <- diff(range(x[], na.rm = TRUE)) * 0.05
#       top <- max(x[], na.rm = TRUE) + offset
#       bot <- min(x[], na.rm = TRUE) - offset
#       values <- seq(bot, top, length.out = 10)
#       values <- round(values, 5)
#     }
#   } else {
#     values <- round(values, 5)
#   }
#
#   if (is.fact) {
#     pal <- leaflet::colorFactor(color,
#                                 domain = NULL,
#                                 na.color = na.color)
#   } else {
#     pal <- leaflet::colorNumeric(color,
#                                  domain = values,
#                                  na.color = na.color)
#   }
#
#   if (use.layer.names) {
#     grp <- names(x)
#   } else {
#     grp <- layer.name
#   }
#
#   ## add layers to base map
#   m <- leaflet::addRasterImage(map = m,
#                                x = x,
#                                colors = pal,
#                                project = FALSE,
#                                opacity = alpha,
#                                group = grp,
#                                ...)
#
#   if (legend) {
#     ## add legend
#     m <- leaflet::addLegend(map = m,
#                             pal = pal,
#                             opacity = legend.opacity,
#                             values = values,
#                             title = grp)
#   }
#
#   m <- mapViewLayersControl(map = m,
#                             map.types = "",
#                             names = grp)
#
#   out <- new('mapview', object = list(x), map = m)
#
#   return(out)
#
# }
#
#
#
# ### plain w RasterStackBrick ==============================================
#
# leafletPlainRSB <- function(x,
#                             map,
#                             maxpixels,
#                             color,
#                             na.color,
#                             values,
#                             legend,
#                             legend.opacity,
#                             trim,
#                             verbose,
#                             ...) {
#
#   pkgs <- c("leaflet", "raster", "magrittr")
#   tst <- sapply(pkgs, "requireNamespace",
#                 quietly = TRUE, USE.NAMES = FALSE)
#
#   if (is.null(map)) m <- leaflet::leaflet() else m <- map
#
#   if (nlayers(x) == 1) {
#     x <- raster(x, layer = 1)
#     m <- plainView(x, map = m,
#                    use.layer.names = TRUE, ...)
#     out <- new('mapview', object = list(x), map = m@map)
#   } else {
#     m <- plainView(x[[1]], map = m,
#                  use.layer.names = TRUE, ...)
#     for (i in 2:nlayers(x)) {
#       m <- plainView(x[[i]], map = m@map,
#                    use.layer.names = TRUE, ...)
#     }
#
#     if (length(getLayerNamesFromMap(m@map)) > 1) {
#       m <- leaflet::hideGroup(map = m@map,
#                               group = layers2bHidden(m@map))
#     }
#     out <- new('mapview', object = list(x), map = m)
#   }
#
#   return(out)
#
# }
#
#
#
# ### plain w SpatialPixelsDataFrame ========================================
#
# leafletPlainPixelsDF <- function(x,
#                                  zcol,
#                                  ...) {
#
#   pkgs <- c("leaflet", "sp", "magrittr")
#   tst <- sapply(pkgs, "requireNamespace",
#                 quietly = TRUE, USE.NAMES = FALSE)
#
#   if(!is.null(zcol)) x <- x[, zcol]
#
#   stck <- do.call("stack", lapply(seq(ncol(x)), function(i) {
#     r <- raster::raster(x[, i])
#     if (is.factor(x[, i])) r <- raster::as.factor(r)
#     return(r)
#   }))
#
#   m <- plainView(stck, ...)
#
#   out <- new('mapview', object = list(x), map = m@map)
#
#   return(out)
#
# }
#
#
#
# ### plain w SpatialPointsDataFrame ========================================
#
# # leafletPlainPointsDF <- function(x,
# #                                  zcol,
# #                                  map,
# #                                  burst,
# #                                  color,
# #                                  na.color,
# #                                  radius,
# #                                  legend,
# #                                  legend.opacity,
# #                                  verbose,
# #                                  layer.name,
# #                                  popup,
# #                                  ...) {
# #
# #   pkgs <- c("leaflet", "sp", "magrittr")
# #   tst <- sapply(pkgs, "requireNamespace",
# #                 quietly = TRUE, USE.NAMES = FALSE)
# #
# #   rad_vals <- circleRadius(x, radius)
# #   if(!is.null(zcol)) x <- x[, zcol]
# #   if(!is.null(zcol)) burst <- TRUE
# #
# #   pop.null <- is.null(popup)
# #
# #   x <- spCheckObject(x, verbose = verbose)
# #   x <- spCheckAdjustProjection(x)
# #
# #   m <- initMap(map, map.types, sp::proj4string(x))
# #
# #   if (burst) {
# #     lst <- lapply(names(x), function(j) x[j])
# #
# #     vals <- lapply(seq(lst), function(i) lst[[i]]@data[, 1])
# #
# #     pal_n <- lapply(seq(lst), function(i) {
# #       if (is.factor(lst[[i]]@data[, 1])) {
# #         leaflet::colorFactor(color, lst[[i]]@data[, 1],
# #                              levels = levels(lst[[i]]@data[, 1]))
# #       } else {
# #         leaflet::colorNumeric(color, vals[[i]],
# #                               na.color = na.color)
# #       }
# #     })
# #
# #     for (i in seq(lst)) {
# #
# #       #x <- lst[[i]]
# #
# #       if (pop.null) popup <- brewPopupTable(lst[[i]])
# #
# #       m <- leaflet::addCircleMarkers(m,
# #                                      lng = coordinates(lst[[i]])[, 1],
# #                                      lat = coordinates(lst[[i]])[, 2],
# #                                      group = names(lst[[i]]),
# #                                      color = pal_n[[i]](vals[[i]]),
# #                                      popup = popup,
# #                                      #data = lst[[i]],
# #                                      radius = rad_vals,
# #                                      ...)
# #
# #       if (legend) {
# #         m <- leaflet::addLegend(map = m, position = "topright",
# #                                 pal = pal_n[[i]],
# #                                 opacity = 1, values = vals[[i]],
# #                                 title = names(lst[[i]]),
# #                                 layerId = names(lst[[i]]))
# #       }
# #
# #     }
# #
# #     m <- mapViewLayersControl(map = m,
# #                               map.types = map.types,
# #                               names = names(x))
# #
# #     #     m <- leaflet::addLayersControl(map = m,
# #     #                                    position = mapviewOptions(
# #     #                                      console = FALSE)$layerscontrolpos,
# #     #                                    baseGroups = map.types,
# #     #                                    overlayGroups = names(x))
# #
# #
# #     if (length(getLayerNamesFromMap(m)) > 1) {
# #       m <- leaflet::hideGroup(map = m, group = layers2bHidden(m))
# #     }
# #
# #   } else {
# #
# #     grp <- layer.name
# #
# #     if (pop.null) popup <- brewPopupTable(x)
# #
# #     m <- leaflet::addCircleMarkers(map = m,
# #                                    lng = coordinates(x)[, 1],
# #                                    lat = coordinates(x)[, 2],
# #                                    group = grp,
# #                                    color = color[length(color)],
# #                                    popup = popup,
# #                                    #data = x,
# #                                    radius = rad_vals,
# #                                    ...)
# #
# #     m <- mapViewLayersControl(map = m,
# #                               map.types = map.types,
# #                               names = grp)
# #
# #     #     m <- leaflet::addLayersControl(map = m,
# #     #                                    position = mapviewOptions(
# #     #                                      console = FALSE)$layerscontrolpos,
# #     #                                    baseGroups = map.types,
# #     #                                    overlayGroups = grp)
# #   }
# #
# #   out <- new('mapview', object = list(x), map = m)
# #
# #   return(out)
# #
# # }
# #
# #
