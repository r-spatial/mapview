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
    leg_clrs <- ifelse(getGeometryType(x) == "ln", color, col.regions)
    legend <- mapviewLegend(values = x[[zcol]],
                            colors = leg_clrs,
                            at = at,
                            na.color = col2Hex(na.color),
                            layer.name = layer.name)
  }

  clrs <- vectorColors(x = x,
                       zcol = zcol,
                       colors = color,
                       at = at,
                       na.color = na.color)
  clrs.regions <- vectorColRegions(x = x,
                                   zcol = zcol,
                                   col.regions = col.regions,
                                   at = at,
                                   na.color = na.color)

  leaflet_sfc(sf::st_geometry(x),
              map = map,
              zcol = zcol,
              color = clrs,
              col.regions = clrs.regions,
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

  if (is.null(map.types)) {
    if (getGeometryType(x) == "pl") {
      if (is.function(col.regions)) col.regions <- standardColRegions(x)
      map.types <- basemaps(col.regions)
    } else {
      if (is.function(color)) color <- standardColor(x)
      map.types <- basemaps(color)
    }
  }

  m <- initMap(map, map.types, sf::st_crs(x), native.crs)

  if (npts(x) > maxpoints) {
    if (getGeometryType(x) == "ln") clrs <- color else clrs <-  col.regions
    m <- addLargeFeatures(m,
                          data = x,
                          radius = cex,
                          weight = lwd,
                          opacity = alpha,
                          fillOpacity = alpha.regions,
                          color = clrs,
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

  funs <- list(if (!native.crs) leaflet::addScaleBar,
               if (homebutton) addHomeButton,
               mapViewLayersControl,
               addMouseCoordinates)
  funs <- funs[!sapply(funs, is.null)]

  args <- list(if (!native.crs) list(position = "bottomleft"),
               if (homebutton) list(ext = createExtent(x),
                                    layer.name = layer.name),
               list(map.types = map.types,
                    names = layer.name,
                    native.crs = native.crs),
               list(style = "detailed",
                    epsg = sf::st_crs(x)$epsg,
                    proj4string = sf::st_crs(x)$proj4string))
  args <- args[!sapply(args, is.null)]

  m <- decorateMap(map = m,
                   funs = funs,
                   args = args)

  if (is.function(legend)) m <- legend(m)
  out <- new("mapview", object = list(x), map = m)

  return(out)

}
