#### SIMPLE FEATURES ######################################################
###########################################################################

### sfc ###################################################################
leaflet_sfc <- function(x,
                        map,
                        cex,
                        lwd,
                        alpha,
                        alpha.regions,
                        color,
                        na.color,
                        map.types,
                        verbose,
                        popup,
                        layer.name,
                        label,
                        legend,
                        legend.opacity,
                        homebutton,
                        ...) {

  m <- initMap(map, map.types, sf::st_crs(x))

  color <- vectorColors(x = x,
                        colors = color,
                        at = at,
                        na.color = na.color)

  m <- addFeatures(m,
                   x,
                   radius = cex,
                   weight = lwd,
                   opacity = alpha,
                   fillOpacity = alpha.regions,
                   color = color,
                   popup = popup,
                   label = label,
                   group = layer.name,
                   ...)

  m <- decorateMap(map = m,
                   funs = list(leaflet::addScaleBar,
                               addHomeButton,
                               mapViewLayersControl),
                   args = list(list(position = "bottomleft"),
                               list(ext = createExtent(x),
                                    layer.name = layer.name),
                               list(map.types = map.types,
                                    names = layer.name,
                                    hasCRS = TRUE)))

  out <- new("mapview", object = list(x), map = m)

  return(out)

}

### sf ####################################################################
leafletSF <- function(x,
                      map,
                      zcol,
                      cex,
                      lwd,
                      alpha,
                      alpha.regions,
                      color,
                      na.color,
                      map.types,
                      verbose,
                      popup,
                      layer.name,
                      label,
                      legend,
                      legend.opacity,
                      homebutton,
                      ...) {


  m <- initMap(map, map.types, sf::st_crs(x))

}







### MULTIPOINT ############################################################

leafletMULTIPOINT <- function(x,
                              map,
                              cex,
                              lwd,
                              alpha,
                              alpha.regions,
                              color,
                              na.color,
                              map.types,
                              verbose,
                              popup,
                              layer.name,
                              label,
                              legend,
                              legend.opacity,
                              homebutton,
                              ...) {

  if(!isAvailableInLeaflet()$lab && verbose) warning(warn)

  m <- initMap(map, map.types, sf::st_crs(x)$proj4string)

  ext <- createExtent(x)

  grp <- layer.name
  if (is.null(label)) label <- makeLabels(x)

  if (homebutton) m <- addHomeButton(m, ext, layer.name = layer.name)

  color <- mapviewColors(x, colors = color)


  if(isAvailableInLeaflet()$lab) {
    m <- leaflet::addCircleMarkers(m,
                                   data = x,
                                   radius = cex,
                                   weight = lwd,
                                   opacity = alpha,
                                   color = color,
                                   fillOpacity = alpha.regions,
                                   group = grp,
                                   label = label,
                                   popup = popup,
                                   ...)
  } else {

    m <- leaflet::addCircleMarkers(m,
                                   data = x,
                                   radius = cex,
                                   weight = lwd,
                                   opacity = alpha,
                                   color = color,
                                   fillOpacity = alpha.regions,
                                   group = grp,
                                   popup = popup,
                                   ...)
  }

  if(!is.na(sf::st_crs(x)$proj4string)) {
    crs <- TRUE
    if (isAvailableInLeaflet()$scl)
      m <- leaflet::addScaleBar(map = m, position = "bottomleft")
    m <- addMouseCoordinates(m, style = "detailed")
  } else {
    crs <- FALSE
    m <- addMouseCoordinates(m, style = "basic")
  }

  m <- mapViewLayersControl(map = m,
                            map.types = map.types,
                            names = grp,
                            hasCRS = crs)

  out <- new('mapview', object = list(x), map = m)

  return(out)

}
