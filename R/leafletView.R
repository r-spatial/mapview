#### functions for leaflet based rendering by spatial class

isAvailableInLeaflet <- function() {
  return(
    list(
      lab = "label" %in% names(as.list(args(leaflet::addCircleMarkers))),
      scl = "addScaleBar" %in% ls(getNamespace("leaflet"))
    )
  )
}

# lab_avl <- isAvailableInLeaflet()$lab
# scl_avl <- isAvailableInLeaflet()$scl

warn <- paste("Feature labels on mouseover and 'addScaleBar' are not supported in the installed version of 'leaflet'.",
              "\nRun devtools::install_github('rstudio/leaflet') and re-install 'mapview' locally to enable these features.")


#### RASTER ###############################################################
###########################################################################
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
                      alpha.regions,
                      legend,
                      legend.opacity,
                      trim,
                      verbose,
                      layer.name,
                      homebutton,
                      ...) {

  pkgs <- c("leaflet", "raster", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  is.fact <- raster::is.factor(x)
  # ext <- raster::extent(raster::projectExtent(x, crs = llcrs))

  m <- initMap(map, map.types, sp::proj4string(x))
  x <- rasterCheckSize(x, maxpixels = maxpixels)
  x <- rasterCheckAdjustProjection(x)
  ext <- raster::extent(raster::projectExtent(x, crs = llcrs))

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
    # pal2 <- pal
  } else {
    pal <- rasterColors(col.regions,
                        at = at,
                        na.color = na.color)

    # if (length(at) > 11) {
    #   pal2 <- leaflet::colorNumeric(palette = col.regions,
    #                                 domain = at,
    #                                 na.color = na.color)
    # } else {
    #   pal2 <- leaflet::colorBin(palette = col.regions,
    #                             bins = length(at),
    #                             domain = at,
    #                             na.color = na.color)
    # }

  }

  if (use.layer.names) {
    grp <- names(x)
    layer.name <- names(x)
  } else {
    grp <- layer.name
  }

  ## add layers to base map
  m <- leaflet::addRasterImage(map = m,
                               x = x,
                               colors = pal,
                               project = FALSE,
                               opacity = alpha.regions,
                               group = grp,
                               ...)

  if (legend) {
    ## add legend
    # m <- leaflet::addLegend(map = m,
    #                         pal = pal2,
    #                         opacity = legend.opacity,
    #                         values = at,
    #                         title = grp)

    m <- addRasterLegend(x = x,
                         map = m,
                         title = grp,
                         at = at,
                         col.regions = col.regions,
                         na.color = na.color)
  }

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  if (isAvailableInLeaflet()$scl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
  m <- addMouseCoordinates(m)

  if (homebutton) m <- addHomeButton(m, ext, layer.name = layer.name)

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
                       use.layer.names,
                       values,
                       map.types,
                       legend,
                       legend.opacity,
                       trim,
                       verbose,
                       layer.name,
                       homebutton,
                       ...) {

  pkgs <- c("leaflet", "raster", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  m <- initMap(map, map.types, sp::proj4string(x))

  if (nlayers(x) == 1) {
    x <- raster(x, layer = 1)
    m <- mapView(x,
                 map = m,
                 maxpixels = maxpixels,
                 map.types = map.types,
                 use.layer.names = use.layer.names,
                 at = at,
                 col.regions = col.regions,
                 na.color = na.color,
                 legend = legend,
                 layer.name = layer.name,
                 homebutton = homebutton,
                 ...)
    out <- new('mapview', object = list(x), map = m@map)
  } else {
    m <- mapView(x[[1]],
                 map = m,
                 maxpixels = maxpixels,
                 map.types = map.types,
                 use.layer.names = use.layer.names,
                 at = at,
                 col.regions = col.regions,
                 na.color = na.color,
                 legend = legend,
                 homebutton = homebutton,
                 ...)
    for (i in 2:nlayers(x)) {
      m <- mapView(x[[i]],
                   map = m@map,
                   maxpixels = maxpixels,
                   map.types = map.types,
                   use.layer.names = use.layer.names,
                   at = at,
                   col.regions = col.regions,
                   na.color = na.color,
                   legend = legend,
                   homebutton = FALSE,
                   ...)
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
                            na.color,
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

  m <- mapView(stck,
               na.color = na.color,
               use.layer.names = TRUE,
               ...)

  out <- new('mapview', object = list(x), map = m@map)

  return(out)

}





### leaflet w Satellite ===================================================

leafletSatellite <- function(x, ...) {

  pkgs <- c("leaflet", "satellite", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  m <- mapView(stack(x), ...)

  out <- new('mapview', object = list(x), map = m@map)

  return(out)

}

###########################################################################
###########################################################################


#### SP ###################################################################
###########################################################################
### leaflet w SpatialPointsDataFrame ======================================

leafletPointsDF <- function(x,
                            map,
                            map.types,
                            zcol,
                            burst,
                            color,
                            alpha,
                            col.regions,
                            alpha.regions,
                            na.color,
                            at,
                            cex,
                            lwd,
                            popup,
                            label,
                            legend,
                            legend.opacity,
                            layer.name,
                            verbose,
                            homebutton,
                            ...) {

  if(!isAvailableInLeaflet()$lab && verbose) warning(warn)

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  cex <- circleRadius(x, cex)
  usr_burst <- burst

  if (is.null(zcol) & is.character(burst)) {
    zcol <- burst
    usr_burst <- TRUE
  }

  if (!is.null(zcol)) {
    x <- x[, zcol]
    burst <- TRUE
  }

  x <- spCheckObject(x)
  if (length(grep("DataFrame", class(x)[1])) == 0) {
    return(leafletPoints(x = x,
                         map = map,
                         cex = cex,
                         lwd = lwd,
                         alpha = alpha,
                         alpha.regions = alpha.regions,
                         na.color = na.color,
                         map.types = map.types,
                         verbose = verbose,
                         layer.name = layer.name,
                         label = label,
                         homebutton = homebutton,
                         ...)
    )
  }

  m <- initMap(map, map.types, sp::proj4string(x))
  x <- spCheckAdjustProjection(x)

  if (length(x) > 1) {
    ext <- raster::extent(raster::projectExtent(x, crs = llcrs))
  } else {
    ext <- extent(xmin(x) - 0.05, xmax(x) + 0.05,
                  ymin(x) - 0.05, ymax(x) + 0.05)
  }

  if (burst) {

    row_nms <- row.names(x)

    leafletList(x,
                map = m,
                map.types = map.types,
                zcol = zcol,
                usr_burst = usr_burst,
                color = color,
                alpha = alpha,
                col.regions = col.regions,
                alpha.regions = alpha.regions,
                na.color = na.color,
                at = at,
                cex = cex,
                lwd = lwd,
                popup = popup,
                label = label,
                legend = legend,
                legend.opacity = legend.opacity,
                layer.name = layer.name,
                verbose = verbose,
                row.nms = row_nms,
                homebutton = homebutton,
                ...)

  } else {

    grp <- layer.name
    if (is.null(label)) label <- makeLabelsSP(row.names(x))

    color <- mapviewColors(x, colors = color, at = at, na.color = na.color)

    if (nrow(x) < mapviewGetOption("maxpoints")) {

      if(isAvailableInLeaflet()$lab) {
        m <- leaflet::addCircleMarkers(map = m,
                                       lng = coordinates(x)[, 1],
                                       lat = coordinates(x)[, 2],
                                       group = grp,
                                       color = color,
                                       weight = lwd,
                                       opacity = alpha,
                                       fillOpacity = alpha.regions,
                                       popup = popup,
                                       label = label,
                                       radius = cex,
                                       ...)
      } else {

        m <- leaflet::addCircleMarkers(map = m,
                                       lng = coordinates(x)[, 1],
                                       lat = coordinates(x)[, 2],
                                       group = grp,
                                       color = color,
                                       weight = lwd,
                                       opacity = alpha,
                                       fillOpacity = alpha.regions,
                                       popup = popup,
                                       radius = cex,
                                       ...)
      }

    } else {
      m <- addLargeFeatures(m,
                            data = x,
                            color = color,
                            weight = lwd,
                            radius = cex,
                            opacity = alpha,
                            fillOpacity = alpha.regions,
                            group = grp,
                            ...)
    }

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = grp)

    if (isAvailableInLeaflet()$scl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m)

    if (homebutton) m <- addHomeButton(m, ext, layer.name = layer.name)

    out <- new('mapview', object = list(x), map = m)

    return(out)
  }
}




### leaflet w SpatialPoints ===============================================

leafletPoints <- function(x,
                          map,
                          cex,
                          lwd,
                          alpha,
                          alpha.regions,
                          color,
                          na.color,
                          map.types,
                          verbose,
                          layer.name,
                          label,
                          homebutton,
                          ...) {

  if(!isAvailableInLeaflet()$lab && verbose) warning(warn)

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  m <- initMap(map, map.types, sp::proj4string(x))
  x <- spCheckAdjustProjection(x)

  if (length(x) > 1) {
    ext <- raster::extent(raster::projectExtent(x, crs = llcrs))
  } else {
    ext <- extent(xmin(x) - 0.05, xmax(x) + 0.05,
                  ymin(x) - 0.05, ymax(x) + 0.05)
  }

  grp <- layer.name
  if (is.null(label)) label <- makeLabelsSP(row.names(x))

  color <- mapviewColors(x, colors = color)

  if (nrow(coordinates(x)) < mapviewGetOption("maxpoints")) {

    if(isAvailableInLeaflet()$lab) {
      m <- leaflet::addCircleMarkers(m,
                                     lng = coordinates(x)[, 1],
                                     lat = coordinates(x)[, 2],
                                     radius = cex,
                                     weight = lwd,
                                     opacity = alpha,
                                     color = color,
                                     fillOpacity = alpha.regions,
                                     group = grp,
                                     label = label,
                                     ...)
    } else {

      m <- leaflet::addCircleMarkers(m,
                                     lng = coordinates(x)[, 1],
                                     lat = coordinates(x)[, 2],
                                     radius = cex,
                                     weight = lwd,
                                     opacity = alpha,
                                     color = color[1],
                                     fillOpacity = alpha.regions,
                                     group = grp,
                                     ...)
    }

  } else {
    m <- addLargeFeatures(m,
                          data = x,
                          color = color,
                          weight = lwd,
                          radius = cex,
                          opacity = alpha,
                          fillOpacity = alpha.regions,
                          group = grp,
                          ...)
  }

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  if (isAvailableInLeaflet()$scl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
  m <- addMouseCoordinates(m)

  if (homebutton) m <- addHomeButton(m, ext, layer.name = layer.name)

  out <- new('mapview', object = list(x), map = m)

  return(out)

}



### leaflet w SpatialPolygonsDataFrame ====================================

leafletPolygonsDF <- function(x,
                              map,
                              map.types,
                              zcol,
                              burst,
                              color,
                              alpha,
                              col.regions,
                              alpha.regions,
                              na.color,
                              at,
                              cex,
                              lwd,
                              popup,
                              label,
                              legend,
                              legend.opacity,
                              layer.name,
                              verbose,
                              homebutton,
                              ...) {

  if(!isAvailableInLeaflet()$lab && verbose) warning(warn)

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  x <- spCheckObject(x)
  if (length(grep("DataFrame", class(x)[1])) == 0) {
    return(leafletPolygons(x = x,
                           map = map,
                           color = color,
                           na.color = na.color,
                           map.types = map.types,
                           lwd = lwd,
                           alpha = alpha,
                           alpha.regions = alpha.regions,
                           verbose = verbose,
                           layer.name = layer.name,
                           label = label,
                           homebutton = homebutton,
                           ...)
    )
  }

  usr_burst <- burst

  if (is.null(zcol) & is.character(burst)) {
    zcol <- burst
    usr_burst <- TRUE
  }

  if (!is.null(zcol)) {
    x <- x[, zcol]
    burst <- TRUE
  }

  m <- initMap(map, map.types, sp::proj4string(x))
  x <- spCheckAdjustProjection(x)

  ext <- raster::extent(raster::projectExtent(x, crs = llcrs))

  if (burst) {

    row_nms <- row.names(x)

    leafletList(x,
                map = m,
                map.types = map.types,
                zcol = zcol,
                usr_burst = usr_burst,
                color = color,
                alpha = alpha,
                col.regions = col.regions,
                alpha.regions = alpha.regions,
                na.color = na.color,
                at = at,
                cex = cex,
                lwd = lwd,
                popup = popup,
                label = label,
                legend = legend,
                legend.opacity = legend.opacity,
                layer.name = layer.name,
                verbose = verbose,
                row.nms = row_nms,
                homebutton = homebutton,
                ...)

  } else {

    grp <- layer.name
    if (is.null(label)) label <- makeLabelsSP(row.names(x))

    color <- mapviewColors(x, colors = color, at = at, na.color = na.color)

    if (isAvailableInLeaflet()$lab) {
      if (length(x@polygons) < mapviewGetOption("maxpolygons")) {
      m <- leaflet::addPolygons(m,
                                weight = lwd,
                                opacity = alpha,
                                fillOpacity = alpha.regions,
                                group = grp,
                                color = color,
                                popup = popup,
                                label = label,
                                data = x,
                                highlightOptions =
                                  mapviewHighlightOptions(x, weight = lwd,
                                                          fillOpacity = alpha.regions),
                                ...)
      } else {
        m <- addLargeFeatures(m,
                              data = x,
                              color = color,
                              weight = lwd,
                              opacity = alpha,
                              fillOpacity = alpha.regions,
                              group = grp,
                              ...)
      }
    } else {

      m <- leaflet::addPolygons(m,
                                weight = lwd,
                                opacity = alpha,
                                fillOpacity = alpha.regions,
                                group = grp,
                                color = color,
                                popup = popup,
                                data = x,
                                highlightOptions =
                                  mapviewHighlightOptions(x, weight = lwd,
                                                          fillOpacity = alpha.regions),
                                ...)
    }

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = grp)

    if (isAvailableInLeaflet()$scl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m)

    if (homebutton) m <- addHomeButton(m, ext, layer.name = layer.name)

    out <- new('mapview', object = list(x), map = m)

    return(out)

  }

}



### leaflet w SpatialPolygons =============================================

leafletPolygons <- function(x,
                            map,
                            color,
                            na.color,
                            map.types,
                            lwd,
                            alpha,
                            alpha.regions,
                            verbose,
                            layer.name,
                            label,
                            homebutton,
                            ...) {

  if(!isAvailableInLeaflet()$lab && verbose) warning(warn)

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  m <- initMap(map, map.types, sp::proj4string(x))
  x <- spCheckAdjustProjection(x)
  ext <- raster::extent(raster::projectExtent(x, crs = llcrs))

  grp <- layer.name
  if (is.null(label)) label <- makeLabelsSP(row.names(x))

  color <- mapviewColors(x, colors = color)

  if (isAvailableInLeaflet()$lab) {
    if (length(x@polygons) < mapviewGetOption("maxpolygons")) {
      m <- leaflet::addPolygons(m,
                                weight = lwd,
                                group = grp,
                                color = color,
                                data = x,
                                opacity = alpha,
                                fillOpacity = alpha.regions,
                                label = label,
                                highlightOptions =
                                  mapviewHighlightOptions(x, weight = lwd,
                                                          fillOpacity = alpha.regions),
                                ...)
    } else {
      m <- addLargeFeatures(m,
                            data = x,
                            color = color,
                            weight = lwd,
                            opacity = alpha,
                            fillOpacity = alpha.regions,
                            group = grp,
                            ...)
    }
  } else {

    m <- leaflet::addPolygons(m,
                              weight = lwd,
                              group = grp,
                              color = color[1],
                              data = x,
                              opacity = alpha,
                              fillOpacity = alpha.regions,
                              highlightOptions =
                                mapviewHighlightOptions(x, weight = lwd,
                                                        fillOpacity = alpha.regions),
                              ...)
  }

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  if (isAvailableInLeaflet()$scl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
  m <- addMouseCoordinates(m)

  if (homebutton) m <- addHomeButton(m, ext, layer.name = layer.name)

  out <- new('mapview', object = list(x), map = m)

  return(out)

}



### leaflet w SpatialLinesDataFrame =======================================

leafletLinesDF <- function(x,
                           map,
                           map.types,
                           zcol,
                           burst,
                           color,
                           alpha,
                           col.regions,
                           alpha.regions,
                           na.color,
                           at,
                           cex,
                           lwd,
                           popup,
                           label,
                           legend,
                           legend.opacity,
                           layer.name,
                           verbose,
                           homebutton,
                           ...) {

  if(!isAvailableInLeaflet()$lab && verbose) warning(warn)

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  usr_burst <- burst

  if (is.null(zcol) & is.character(burst)) {
    zcol <- burst
    usr_burst <- TRUE
  }

  if (!is.null(zcol)) {
    x <- x[, zcol]
    burst <- TRUE
  }

  x <- spCheckObject(x)
  if (length(grep("DataFrame", class(x)[1])) == 0) {
    return(leafletLines(x = x,
                        map = map,
                        color = color,
                        na.color = na.color,
                        map.types = map.types,
                        lwd = lwd,
                        alpha = alpha,
                        verbose = verbose,
                        layer.name = layer.name,
                        label = label,
                        homebutton = homebutton,
                        ...)
    )
  }

  m <- initMap(map, map.types, sp::proj4string(x))
  x <- spCheckAdjustProjection(x)
  ext <- raster::extent(raster::projectExtent(x, crs = llcrs))

  if (burst) {

    row_nms <- row.names(x)

    leafletList(x,
                map = m,
                map.types = map.types,
                zcol = zcol,
                usr_burst = usr_burst,
                color = color,
                alpha = alpha,
                col.regions = col.regions,
                alpha.regions = alpha.regions,
                na.color = na.color,
                at = at,
                cex = cex,
                lwd = lwd,
                popup = popup,
                label = label,
                legend = legend,
                legend.opacity = legend.opacity,
                layer.name = layer.name,
                verbose = verbose,
                row.nms = row_nms,
                homebutton = homebutton,
                ...)

  } else {

    grp <- layer.name
    if (is.null(label)) label <- makeLabelsSP(row.names(x))

    if (missing(popup)) popup <- brewPopupTable(x)

    color <- mapviewColors(x, colors = color, at = at, na.color = na.color)
    if (is.null(zcol) && !usr_burst) color <- rep(color, length(x))

    ### test -----

    if (length(x@lines) < mapviewGetOption("maxlines")) {

      for (i in 1:length(x)) {

        # individual popup
        #if (missing(popup)) popup <- brewPopupTable(x[i, ])

        # continuous line
        segments <- length(x[i, ]@lines[[1]]@Lines)

        if (isAvailableInLeaflet()$lab) {
          if (segments == 1) {
            m <- leaflet::addPolylines(m,
                                       group = grp,
                                       color = color[i],
                                       popup = popup[i],
                                       label = label[i],
                                       data = x[i, ],
                                       weight = lwd,
                                       opacity = alpha,
                                       highlightOptions =
                                         mapviewHighlightOptions(x[i, ],
                                                                 weight = lwd,
                                                                 fill = FALSE),
                                       ...)

            # disjunct line
          } else {

            # add one segment after another
            for (j in seq(segments)) {

              col <- rep(color[i], length(segments[i]))

              # when dealing with a single-column data.frame, argument 'data'
              # passed on to sp::SpatialLinesDataFrame needs to be defined
              # manually as data.frame with uniform column and row names
              dat <- x@data[i, ]
              if (!is.data.frame(dat)) {
                dat <- data.frame(dat)
                names(dat) <- names(x@data)
                rownames(dat) <- rownames(x@data)[i]
              }

              slndf <- coords2Lines(x[i, ]@lines[[1]]@Lines[[j]]
                                    , ID = rownames(x@data)[i]
                                    , data = dat
                                    , proj4string = sp::CRS(sp::proj4string(x)))

              m <- leaflet::addPolylines(m,
                                         group = grp,
                                         color = col[i],
                                         popup = popup[i],
                                         label = label[i],
                                         data = slndf,
                                         weight = lwd,
                                         opacity = alpha,
                                         highlightOptions =
                                           mapviewHighlightOptions(slndf, weight = lwd,
                                                                   fill = FALSE),
                                         ...)
            }
          }



        } else {
          if (segments == 1) {
            m <- leaflet::addPolylines(m,
                                       group = grp,
                                       color = color[i],
                                       popup = popup[i],
                                       data = x[i, ],
                                       weight = lwd,
                                       opacity = alpha,
                                       highlightOptions =
                                         mapviewHighlightOptions(x[i, ], weight = lwd,
                                                                 fill = FALSE),
                                       ...)

            # disjunct line
          } else {

            # add one segment after another
            for (j in seq(segments)) {

              dat <- x@data[i, ]
              if (!is.data.frame(dat)) {
                dat <- data.frame(dat)
                names(dat) <- names(x@data)
                rownames(dat) <- rownames(x@data)[i]
              }

              slndf <- coords2Lines(x[i, ]@lines[[1]]@Lines[[j]]
                                    , ID = rownames(x@data)[i]
                                    , data = dat
                                    , proj4string = sp::CRS(sp::proj4string(x)))

              m <- leaflet::addPolylines(m,
                                         group = grp,
                                         color = color,
                                         popup = popup[i],
                                         data = slndf,
                                         weight = lwd,
                                         opacity = alpha,
                                         highlightOptions =
                                           mapviewHighlightOptions(slndf, weight = lwd,
                                                                   fill = FALSE),
                                         ...)
            }
          }
        }
      }
    } else {
      m <- addLargeFeatures(m,
                            data = x,
                            color = color,
                            weight = lwd,
                            opacity = alpha,
                            group = grp,
                            ...)
    }

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = grp)

    if (isAvailableInLeaflet()$scl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m)

    if (homebutton) m <- addHomeButton(m, ext, layer.name = layer.name)

    out <- new('mapview', object = list(x), map = m)

    return(out)
  }

}



### leaflet w SpatialLines ================================================

leafletLines <- function(x,
                         map,
                         color,
                         na.color,
                         map.types,
                         lwd,
                         alpha,
                         verbose,
                         layer.name,
                         label,
                         homebutton,
                         ...) {

  if(!isAvailableInLeaflet()$lab && verbose) warning(warn)

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  #llcrs <- CRS("+init=epsg:4326")@projargs

  m <- initMap(map, map.types, sp::proj4string(x))
  x <- spCheckAdjustProjection(x)
  ext <- raster::extent(raster::projectExtent(x, crs = llcrs))

  grp <- layer.name
  if (is.null(label)) label <- makeLabelsSP(row.names(x))

  color <- mapviewColors(x, colors = color)

  ### test -----
  if (length(x@lines) < mapviewGetOption("maxlines")) {
    if(isAvailableInLeaflet()$lab) {
      for (i in 1:length(x)) {

        # continuous line
        segments <- length(x[i, ]@lines[[1]]@Lines)

        if (segments == 1) {
          m <- leaflet::addPolylines(m,
                                     group = grp,
                                     color = color,
                                     data = x[i, ],
                                     weight = lwd,
                                     opacity = alpha,
                                     label = label[i],
                                     ...)

          # disjunct line
        } else {

          # add one segment after another
          for (j in seq(segments)) {
            ln <- x[i, ]@lines[[1]]@Lines[[j]]
            lns <- sp::Lines(list(ln), ID = i)
            sln <- sp::SpatialLines(list(lns),
                                    proj4string = sp::CRS(sp::proj4string(x)))
            m <- leaflet::addPolylines(m,
                                       group = grp,
                                       color = color,
                                       data = sln,
                                       weight = lwd,
                                       opacity = alpha,
                                       label = label[i],
                                       ...)
          }
        }
      }
    } else {
      for (i in 1:length(x)) {

        # continuous line
        segments <- length(x[i, ]@lines[[1]]@Lines)

        if (segments == 1) {
          m <- leaflet::addPolylines(m,
                                     group = grp,
                                     color = color,
                                     data = x[i, ],
                                     weight = lwd,
                                     opacity = alpha,
                                     ...)

          # disjunct line
        } else {

          # add one segment after another
          for (j in seq(segments)) {
            ln <- x[i, ]@lines[[1]]@Lines[[j]]
            lns <- sp::Lines(list(ln), ID = i)
            sln <- sp::SpatialLines(list(lns),
                                    proj4string = sp::CRS(sp::proj4string(x)))
            m <- leaflet::addPolylines(m,
                                       group = grp,
                                       color = color,
                                       data = sln,
                                       weight = lwd,
                                       opacity = alpha,
                                       ...)
          }
        }
      }
    }

  } else {
    m <- addLargeFeatures(m,
                          data = x,
                          color = color,
                          weight = lwd,
                          opacity = alpha,
                          group = grp,
                          ...)
  }

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  if (isAvailableInLeaflet()$scl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
  m <- addMouseCoordinates(m)

  if (homebutton) m <- addHomeButton(m, ext, layer.name = layer.name)

  out <- new('mapview', object = list(x), map = m)

  return(out)

}

###########################################################################
###########################################################################




#### MISC #################################################################
###########################################################################
### leaflet w list ========================================================

leafletList <- function(x,
                        map,
                        map.types,
                        zcol,
                        usr_burst,
                        color,
                        alpha,
                        col.regions,
                        alpha.regions,
                        na.color,
                        at,
                        cex,
                        lwd,
                        popup,
                        label,
                        legend,
                        legend.opacity,
                        layer.name,
                        verbose,
                        row.nms,
                        homebutton,
                        ...) {

  # if (is.factor(x@data[, zcol])) {
  #   vals <- as.character(x@data[, zcol])
  # } else {
  #   vals <- x@data[, zcol] # orig values needed for legend creation later on
  # }
  # is.fact <- is.factor(x@data[, zcol])
  pop <- popup
  #cls <- class(x)[1]
  bbr <- length(zcol) == 1L && usr_burst

  if(bbr) {

    #map <- initBaseMaps(map.types = map.types)
    #map <- initMap(map, map.types, sp::proj4string(x))

    if (legend) {
      map <- addVectorLegend(x,
                             map = map,
                             zcol = zcol,
                             at = at,
                             col.regions = col.regions,
                             na.color = na.color)
    }

    x@data[, zcol] <- as.factor(x@data[, zcol])
    lst <- split(x, x@data[, zcol])
    col <- mapviewColors(lst, colors = color, at = at, na.color = na.color)
    if (length(cex) == 1 & is.numeric(cex)) cex <- rep(cex, length(x))
    if (length(layer.name) < length(lst)) {
      layer.name <- sapply(seq(lst), function(i) paste(zcol, names(lst)[i]))
    }

    m <- Reduce("+", lapply(seq(lst), function(i) {
      ind <- which(row.nms %in% row.names(lst[[i]]))
      pop <- popup[ind]

      mapView(x = lst[[i]],
              map = map,
              map.types = map.types,
              zcol = NULL,
              burst = FALSE,
              color = col[[i]],
              alpha = alpha,
              col.regions = col.regions,
              alpha.regions = alpha.regions,
              na.color = na.color,
              at = at,
              cex = cex[ind],
              lwd = lwd,
              popup = pop,
              label = makeLabelsSP(lst[[i]]@data[, 1]),
              legend = legend,
              legend.opacity = legend.opacity,
              layer.name = layer.name[i],
              verbose = verbose,
              homebutton = homebutton,
              ...)
    }))

  } else {

    lst <- lapply(names(x), function(j) x[j])
    zcol <- names(x)
    col <- vector("list", length(lst))
    for (i in seq(lst)) {
      col[[i]] <- mapviewColors(lst[[i]], zcol = zcol[i],
                                colors = color, at = at,
                                na.color = na.color)
    }

    if (length(layer.name) < length(lst) |
        length(utils::find(layer.name[1], mode = "S4")) > 0) {
      layer.name <- paste(layer.name, names(x))
    }

    #map <- initBaseMaps(map.types = map.types)
    #map <- initMap(map, map.types, sp::proj4string(x))

    m <- Reduce("+", lapply(seq(lst), function(i) {
      ind <- which(row.nms %in% row.names(lst[[i]]))
      pop <- popup[ind]

      if (legend) {
        map <- addVectorLegend(lst[[i]],
                               map = map,
                               zcol = zcol[i],
                               at = at,
                               col.regions = col.regions,
                               na.color = na.color)
      }

      mapView(x = lst[[i]],
              map = map,
              map.types = map.types,
              zcol = NULL,
              burst = FALSE,
              color = col[[i]],
              alpha = alpha,
              col.regions = col.regions,
              alpha.regions = alpha.regions,
              na.color = na.color,
              at = at,
              cex = cex,
              lwd = lwd,
              popup = pop,
              label = makeLabelsSP(lst[[i]]@data[, 1]),
              legend = legend,
              legend.opacity = legend.opacity,
              layer.name = layer.name[i],
              verbose = verbose,
              homebutton = FALSE,
              ...)
    }))

  }

  if (!bbr && length(getLayerNamesFromMap(m@map)) > 1) {
    m@map <- leaflet::hideGroup(map = m@map, group = layers2bHidden(m@map))
  }

  if (!bbr & homebutton) {
    ln <- strsplit(layer.name[1], " ")[[1]][1]
    m@map <- addHomeButton(m@map,
                           ext = raster::extent(x),
                           layer.name = ln)
  }

  if (bbr) {
    m@map <- fitBounds(m@map,
                       raster::xmin(x),
                       raster::ymin(x),
                       raster::xmax(x),
                       raster::ymax(x))
  }

  return(m)

}


### leaflet w missing =====================================================

leafletMissing <- function(map.types,
                           easter.egg) {

  if(easter.egg) {

    tilesets <- list(
      ts = c(
        'http://{s}.tiles.mapbox.com/v3/gvenech.m13knc8e/{z}/{x}/{y}.png'
        # , 'http://{s}.tile.thunderforest.com/pioneer/{z}/{x}/{y}.png'
        # , 'http://a.gps-tile.openstreetmap.org/lines/{z}/{x}/{y}.png'
        # , 'http://a.tile.openstreetmap.fr/hot/{z}/{x}/{y}.png'
        # , 'http://a.tiles.wmflabs.org/hikebike/{z}/{x}/{y}.png'
        ),
      attr = c(
        '&copy; <a href="https://www.mapbox.com/map-feedback/">Mapbox</a> &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a> <strong><a href="https://www.mapbox.com/map-feedback/" target="_blank">Improve this map</a></strong>'
        # , '&copy; <a href="http://www.thunderforest.com/">Thunderforest</a>, &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>'
        # , '<a href="https://openstreetmap.org/"> &copy; OpenStreetMap contributors, CC-BY-SA</a>'
        # , '<a href="http://hot.openstreetmap.org/"> &copy; OpenStreetMap contributors, tiles courtesy of Humanitarian OpenStreetMap Team</a>'
        # , '<a href="http://hikebikemap.org/"> &copy; OpenStreetMap contributors, CC-BY-SA</a>'
        )
    )

    ind <- sample(length(tilesets$ts), 1L)
    ts <- tilesets$ts[ind]
    attr <- tilesets$attr[ind]

    envinMR <- data.frame(x = 8.771676,
                          y = 50.814891,
                          envinMR = "envinMR")
    sp::coordinates(envinMR) <- ~x+y
    sp::proj4string(envinMR) <- sp::CRS(llcrs)
    m <- leaflet() %>%
      addTiles(urlTemplate = ts,
               attribution = attr,
               options = tileOptions(minZoom = 1, maxZoom = 18))

    fl <- 'http://cdn.makeagif.com/media/8-11-2015/n2JwUG.gif'

    cit <- unclass(utils::citation("mapview", auto = NULL))[[1]]
    cit <- attr(cit, "textVersion")
    cit <- paste(strsplit(cit, "\\. ")[[1]][1:3], collapse = ". ")

    pop <- paste("<center>", "<b>",
                 '<a target="_blank" href="http://environmentalinformatics-marburg.github.io/mapview/introduction.html">mapview</a>',
                 "</b>", "<br>",
                 " was created at",
                 "<br>",
                 '<a target="_blank" href="http://environmentalinformatics-marburg.de/">Environmental Informatics Marburg</a>',
                 "<br>", "by ", "<br>",
                 '<a target="_blank" href="https://github.com/tim-salabim">Tim Appelhans</a>',
                 "<br>", "<br>",
                 '<hr width=50% style="border: none; height: 1px; color: #D8D8D8; background: #D8D8D8;"/>',
                 "<br>",
                 "Please cite as: ", "<br>",
                 cit,
                 "<br>", "<br>",
                 '<hr width=50% style="border: none; height: 1px; color: #D8D8D8; background: #D8D8D8;"/>',
                 "<br>",
                 "<b>", "mapview", "</b>", "is for quick visualisation of spatial data",
                 "<br>", "<br>",
                 paste('<img src =', fl, 'width="95%">'),
                 '<a target="_blank" href="http://makeagif.com/n2JwUG">Source: MakeAGIF.com</a>',
                 "</center>")
    m <- leaflet::addCircleMarkers(data = envinMR, map = m,
                                   fillColor = "cyan",
                                   color = "black",
                                   radius = 15,
                                   weight = 4,
                                   opacity = 0.8,
                                   fillOpacity = 0.8,
                                   group = "envinMR",
                                   popup = pop)
    m <- leaflet::addPopups(map = m,
                            lng = 8.771676,
                            lat = 50.814891,
                            popup = pop,
                            options = popupOptions(closeOnClick = TRUE))
    m <- leaflet::addLayersControl(map = m,
                                   position = "topleft",
                                   overlayGroups = "envinMR")
    m <- leaflet::setView(map = m, 8.771676, 50.814891, zoom = 4)
    if (isAvailableInLeaflet()$scl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m) %>% addHomeButton(extent(envinMR),
                                                  "mapview home")
    out <- new('mapview', object = list(NULL), map = m)
  } else {
    m <- initBaseMaps(map.types)
    m <- leaflet::setView(map = m, 8.770862, 50.814772, zoom = 18)
    m <- leaflet::addLayersControl(map = m,
                                   baseGroups = map.types,
                                   position = mapviewGetOption(
                                     "layers.control.pos"))
    if (isAvailableInLeaflet()$scl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m)
    out <- new('mapview', object = list(NULL), map = m)
  }
  return(out)

}

###########################################################################
###########################################################################


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
