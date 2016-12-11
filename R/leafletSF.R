#### SIMPLE FEATURES ######################################################
###########################################################################
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
