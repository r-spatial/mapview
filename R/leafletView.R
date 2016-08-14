#### functions for leaflet based rendering by spatial class

lab_avl <- "label" %in% names(as.list(args(leaflet::addCircleMarkers)))
scl_avl <- "addScaleBar" %in% ls(getNamespace("leaflet"))

warn <- paste("Feature labels on mouseover and 'addScaleBar' are not supported in the installed version of 'leaflet'.",
              "\nRun devtools::install_github('rstudio/leaflet') and re-install 'mapview' locally to enable these features.")

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

  if (scl_avl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
  m <- addMouseCoordinates(m)
  m <- addHomeButton(m, ext, layer.name = layer.name)

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
                            ...) {

  if(!lab_avl && verbose) warning(warn)

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
                ...)

  } else {

    grp <- layer.name
    if (missing(label)) label <- makeLabels(row.names(x))

    color <- mapviewColors(x, colors = color, at = at, na.color = na.color)

    if(lab_avl) {
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

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = grp)

    if (scl_avl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m)
    m <- addHomeButton(m, ext, layer.name = layer.name)

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
                          ...) {

  if(!lab_avl && verbose) warning(warn)

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  x <- spCheckAdjustProjection(x)
  ext <- raster::extent(raster::projectExtent(x, crs = llcrs))
  m <- initMap(map, map.types, sp::proj4string(x))

  grp <- layer.name
  label <- makeLabels(row.names(x))

  color <- mapviewColors(x, colors = color)

  if(lab_avl) {
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

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  if (scl_avl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
  m <- addMouseCoordinates(m)
  m <- addHomeButton(m, ext, layer.name = layer.name)

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
                              ...) {

  if(!lab_avl && verbose) warning(warn)

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
                ...)

  } else {

    grp <- layer.name
    if (missing(label)) label <- makeLabels(row.names(x))

    color <- mapviewColors(x, colors = color, at = at, na.color = na.color)

    if (lab_avl) {
      m <- leaflet::addPolygons(m,
                                weight = lwd,
                                opacity = alpha,
                                fillOpacity = alpha.regions,
                                group = grp,
                                color = color,
                                popup = popup,
                                label = label,
                                data = x,
                                ...)
    } else {

      m <- leaflet::addPolygons(m,
                                weight = lwd,
                                opacity = alpha,
                                fillOpacity = alpha.regions,
                                group = grp,
                                color = color,
                                popup = popup,
                                data = x,
                                ...)
    }

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = grp)

    if (scl_avl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m)
    m <- addHomeButton(m, ext, layer.name = layer.name)

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
                            ...) {

  if(!lab_avl && verbose) warning(warn)

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  m <- initMap(map, map.types, sp::proj4string(x))
  x <- spCheckAdjustProjection(x)
  ext <- raster::extent(raster::projectExtent(x, crs = llcrs))

  grp <- layer.name
  if (missing(label)) label <- makeLabels(row.names(x))

  color <- mapviewColors(x, colors = color)

  if (lab_avl) {
    m <- leaflet::addPolygons(m,
                              weight = lwd,
                              group = grp,
                              color = color,
                              data = x,
                              opacity = alpha,
                              fillOpacity = alpha.regions,
                              label = label,
                              ...)
  } else {

    m <- leaflet::addPolygons(m,
                              weight = lwd,
                              group = grp,
                              color = color[1],
                              data = x,
                              opacity = alpha,
                              fillOpacity = alpha.regions,
                              ...)
  }

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  if (scl_avl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
  m <- addMouseCoordinates(m)
  m <- addHomeButton(m, ext, layer.name = layer.name)

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
                           ...) {

  if(!lab_avl && verbose) warning(warn)

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
                ...)

  } else {

    grp <- layer.name
    if (missing(label)) label <- makeLabels(row.names(x))

    if (missing(popup)) popup <- brewPopupTable(x)

    color <- mapviewColors(x, colors = color, at = at, na.color = na.color)
    if (is.null(zcol) && !usr_burst) color <- rep(color, length(x))

    ### test -----

    for (i in 1:length(x)) {

      # individual popup
      #if (missing(popup)) popup <- brewPopupTable(x[i, ])

      # continuous line
      segments <- length(x[i, ]@lines[[1]]@Lines)

      if (lab_avl) {
        if (segments == 1) {
          m <- leaflet::addPolylines(m,
                                     group = grp,
                                     color = color[i],
                                     popup = popup[i],
                                     label = label[i],
                                     data = x[i, ],
                                     weight = lwd,
                                     opacity = alpha,
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
                                       ...)
          }
        }
      }
    }

    m <- mapViewLayersControl(map = m,
                              map.types = map.types,
                              names = grp)

    if (scl_avl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m)
    m <- addHomeButton(m, ext, layer.name = layer.name)

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
                         ...) {

  if(!lab_avl && verbose) warning(warn)

  pkgs <- c("leaflet", "sp", "magrittr")
  tst <- sapply(pkgs, "requireNamespace",
                quietly = TRUE, USE.NAMES = FALSE)

  #llcrs <- CRS("+init=epsg:4326")@projargs

  m <- initMap(map, map.types, sp::proj4string(x))
  x <- spCheckAdjustProjection(x)
  ext <- raster::extent(raster::projectExtent(x, crs = llcrs))

  grp <- layer.name
  if (missing(label)) label <- makeLabels(row.names(x))

  color <- mapviewColors(x, colors = color)

  ### test -----

  if(lab_avl) {
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

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp)

  if (scl_avl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
  m <- addMouseCoordinates(m)
  m <- addHomeButton(m, ext, layer.name = layer.name)

  out <- new('mapview', object = list(x), map = m)

  return(out)

}


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
              label = makeLabels(lst[[i]]@data[, 1]),
              legend = legend,
              legend.opacity = legend.opacity,
              layer.name = layer.name[i],
              verbose = verbose,
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
        length(find(layer.name[1], mode = "S4")) > 0) {
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
              label = makeLabels(lst[[i]]@data[, 1]),
              legend = legend,
              legend.opacity = legend.opacity,
              layer.name = layer.name[i],
              verbose = verbose,
              ...)
    }))
  }

  if (!bbr && length(getLayerNamesFromMap(m@map)) > 1) {
    m@map <- leaflet::hideGroup(map = m@map, group = layers2bHidden(m@map))
  }

  if (bbr) {
    m@map <- fitBounds(m@map,
                       raster::xmin(x),
                       raster::ymin(x),
                       raster::xmax(x),
                       raster::ymax(x))
  }

  # if (legend) {
  #
  #   if (is.null(at)) {
  #     if (is.fact) {
  #       at <- vals
  #     } else {
  #       at <- lattice::do.breaks(range(vals,
  #                                      na.rm = TRUE),
  #                                length(vals))
  #     }
  #   }
  #
  #   if (is.fact) {
  #     pal <- leaflet::colorFactor(palette = col.regions(length(at)),
  #                                 domain = at,
  #                                 na.color = col2Hex(na.color))
  #     pal2 <- pal
  #   } else {
  #     pal <- rasterColors(col.regions,
  #                                   at = at,
  #                                   na.color = col2Hex(na.color))
  #
  #     if (length(at) > 11) {
  #       pal2 <- leaflet::colorNumeric(palette = col.regions(length(at)),
  #                                     domain = at,
  #                                     na.color = col2Hex(na.color))
  #     } else {
  #       pal2 <- leaflet::colorBin(palette = col.regions(length(at)),
  #                                 bins = at, #length(at),
  #                                 domain = at,
  #                                 na.color = col2Hex(na.color))
  #     }
  #
  #   }
  #   if (any(is.na(vals))) {
  #     leg_vals <- c(at, NA)
  #   } else leg_vals <- at
  #
  #   m@map <- leaflet::addLegend(map = m@map,
  #                               position = "topright",
  #                               values = leg_vals,
  #                               pal = pal2,
  #                               opacity = 1,
  #                               labFormat = labelFormat(big.mark = ""),
  #                               title = zcol)
  #
  # }

  return(m)

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

    fl <- 'http://cdn.makeagif.com/media/8-11-2015/n2JwUG.gif'

    pop <- paste("<center>", "<b>", "mapview", "</b>", "<br>", " was created at",
                 "<br>",
                 '<a target="_blank" href="http://environmentalinformatics-marburg.de/">Environmental Informatics Marburg</a>',
                 "<br>", "by ", "<br>",
                 '<a target="_blank" href="http://www.uni-marburg.de/fb19/fachgebiete/umweltinformatik/appelhanst/index.html">Tim Appelhans</a>',
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
                 "<br>", "<br>",
                 '<hr width=50% style="border: none; height: 1px; color: #D8D8D8; background: #D8D8D8;"/>',
                 "<br>",
                 "<b>", "mapview", "</b>", "is for quick visualisation of spatial data",
                 "<br>", "<br>",
                 paste('<img src =', fl, 'width="95%">'),
                 '<a target="_blank" href="http://makeagif.com/n2JwUG">Source: MakeAGIF.com</a>',
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
    if (scl_avl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m)
    out <- new('mapview', object = list(NULL), map = m)
  } else {
    m <- initBaseMaps(map.types)
    m <- leaflet::setView(map = m, 8.770862, 50.814772, zoom = 18)
    m <- leaflet::addLayersControl(map = m, baseGroups = map.types,
                                   position = mapviewGetOption(
                                     "layers.control.pos"))
    if (scl_avl) m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m)
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
# #   x <- spCheckObject(x)
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
# #                                    color = color[1],
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
