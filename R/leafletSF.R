### sf ####################################################################
leaflet_sf <- function(x,
                       map,
                       zcol,
                       cex,
                       lwd,
                       alpha,
                       alpha.regions,
                       color,
                       col.regions,
                       at,
                       na.color,
                       map.types,
                       verbose,
                       popup,
                       layer.name,
                       label,
                       legend,
                       legend.opacity,
                       homebutton,
                       native.crs,
                       highlightOptions,
                       maxpoints,
                       ...) {
### 1. if zcol -> x <- x[, zcol]
### 2. if burst -> burst(x)
  # if (length(zcol) > 1) {
  #   tst <- burst(x[, zcol])
  #   ind <- which(names(tst) == attr(x, "sf_column"))
  #   tst <- tst[-ind]
  #   mapview(tst,
  #           map = map,
  #           zcol = zcol,
  #           color = color,
  #           col.regions = col.regions,
  #           at = at,
  #           na.color = na.color,
  #           cex = cex,
  #           lwd = lwd,
  #           alpha = alpha,
  #           alpha.regions = alpha.regions,
  #           map.types = map.types,
  #           verbose = verbose,
  #           popup = popup,
  #           layer.name = layer.name,
  #           label = label,
  #           legend = legend,
  #           legend.opacity = legend.opacity,
  #           homebutton = homebutton,
  #           native.crs = native.crs,
  #           highlightOptions = highlightOptions,
  #           maxpoints = maxpoints,
  #           ...)
  # } else {

  if (!is.null(zcol)) {
    layer.name <- paste(layer.name, zcol)
    if (length(unique(x[[zcol]])) <= 1) {
      warning(
        sprintf(
          "column %s has only one unique value/level, ignoring coloring and legend",
          zcol
        )
      )
      zcol <- NULL
    }
  }

  if (!native.crs) x <- checkAdjustProjection(x)
  if (legend & !is.null(zcol)) {
    legend <- mapviewLegend(values = x[[zcol]],
                            colors = color,
                            at = at,
                            na.color = col2Hex(na.color),
                            layer.name = layer.name)
  }

  clrs <- vectorColors(x = x,
                       zcol = zcol,
                       colors = color,
                       at = at,
                       na.color = na.color)

  leaflet_sfc(sf::st_geometry(x),
              map = map,
              zcol = zcol,
              color = clrs,
              col.regions = clrs,
              at = at,
              na.color = na.color,
              cex = cex,
              lwd = lwd,
              alpha = alpha,
              alpha.regions = alpha.regions,
              map.types = map.types,
              verbose = verbose,
              popup = popup,
              layer.name = layer.name,
              label = label,
              legend = legend,
              legend.opacity = legend.opacity,
              homebutton = homebutton,
              native.crs = native.crs,
              highlightOptions = highlightOptions,
              maxpoints = maxpoints,
              attributes = sf2DataFrame(x, remove_sf_column = TRUE),
              ...)

}


### sfc ###################################################################
leaflet_sfc <- function(x,
                        map,
                        zcol,
                        cex,
                        lwd,
                        alpha,
                        alpha.regions,
                        color,
                        col.regions,
                        at,
                        na.color,
                        map.types,
                        verbose,
                        popup,
                        layer.name,
                        label,
                        legend,
                        legend.opacity,
                        homebutton,
                        native.crs,
                        highlightOptions,
                        maxpoints,
                        attributes = NULL,
                        ...) {

  if (!native.crs) x <- checkAdjustProjection(x)

  if (is.null(map.types)) map.types <- basemaps(color)

  m <- initMap(map, map.types, sf::st_crs(x), native.crs)

  if (npts(x) > maxpoints) {
    m <- addLargeFeatures(m,
                          data = x,
                          radius = cex,
                          weight = lwd,
                          opacity = alpha,
                          fillOpacity = alpha.regions,
                          color = color,
                          filColor = col.regions,
                          popup = popup,
                          label = label,
                          group = layer.name,
                          maxpoints = maxpoints,
                          attributes = attributes,
                          ...)

  } else {

  m <- addFeatures(m,
                   data = x,
                   radius = cex,
                   weight = lwd,
                   opacity = alpha,
                   fillOpacity = alpha.regions,
                   color = color,
                   fillColor = col.regions,
                   popup = popup,
                   label = label,
                   group = layer.name,
                   highlightOptions = highlightOptions,
                   ...)

  }

  m <- decorateMap(map = m,
                   funs = list(if (!native.crs) leaflet::addScaleBar,
                               if (homebutton) addHomeButton,
                               mapViewLayersControl,
                               addMouseCoordinates),
                   args = list(if (!native.crs) list(position = "bottomleft"),
                               if (homebutton) list(ext = createExtent(x),
                                                    layer.name = layer.name),
                               list(map.types = map.types,
                                    names = layer.name,
                                    native.crs = native.crs),
                               list(style = "detailed",
                                    epsg = sf::st_crs(x)$epsg,
                                    proj4string = sf::st_crs(x)$proj4string)))

  if (is.function(legend)) m <- legend(m)
  out <- new("mapview", object = list(x), map = m)

  return(out)

}
