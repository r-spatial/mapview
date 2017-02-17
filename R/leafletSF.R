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
                          weight = 1,
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
                               addHomeButton,
                               mapViewLayersControl,
                               addMouseCoordinates),
                   args = list(if (!native.crs) list(position = "bottomleft"),
                               list(ext = createExtent(x),
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

  if (!native.crs) x <- checkAdjustProjection(x)
  if (legend & !is.null(zcol)) {
    legend <- mapviewLegend(values = x[[zcol]],
                            colors = color,
                            at = at,
                            na.color = col2Hex(na.color))
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


### large #################################################################
# leaflet_large <- function(x,
#                           map,
#                           zcol,
#                           cex,
#                           lwd,
#                           alpha,
#                           alpha.regions,
#                           color,
#                           at,
#                           na.color,
#                           map.types,
#                           verbose,
#                           popup,
#                           layer.name,
#                           label,
#                           legend,
#                           legend.opacity,
#                           homebutton,
#                           native.crs,
#                           ...) {
#
#   if (!native.crs) x <- checkAdjustProjection(x)
#
#   m <- initMap(map, map.types, sf::st_crs(x), native.crs)
#
#   m <- addLargeFeatures(m,
#                         data = x,
#                         radius = cex,
#                         weight = lwd,
#                         opacity = alpha,
#                         fillOpacity = alpha.regions,
#                         color = color,
#                         popup = popup,
#                         label = label,
#                         group = layer.name,
#                         ...)
#
#   m <- decorateMap(map = m,
#                    funs = list(if (!native.crs) leaflet::addScaleBar,
#                                addHomeButton,
#                                mapViewLayersControl,
#                                addMouseCoordinates),
#                    args = list(if (!native.crs) list(position = "bottomleft"),
#                                list(ext = createExtent(x),
#                                     layer.name = layer.name),
#                                list(map.types = map.types,
#                                     names = layer.name,
#                                     native.crs = native.crs),
#                                list(style = "detailed",
#                                     epsg = sf::st_crs(x)$epsg,
#                                     proj4string = sf::st_crs(x)$proj4string)))
#
#   out <- new("mapview", object = list(x), map = m)
#
#   return(out)
#
# }
