### sfc ###################################################################
leaflet_sfc <- function(x,
                        map,
                        zcol,
                        cex,
                        lwd,
                        alpha,
                        alpha.regions,
                        color,
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
                        ...) {

  if (!native.crs) x <- checkAdjustProjection(x)

  m <- initMap(map, map.types, sf::st_crs(x), native.crs)

  m <- addFeatures(m,
                   data = x,
                   radius = cex,
                   weight = lwd,
                   opacity = alpha,
                   fillOpacity = alpha.regions,
                   color = color,
                   popup = popup,
                   label = label,
                   group = layer.name,
                   highlightOptions = highlightOptions,
                   ...)

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
                       ...) {

  if (!native.crs) x <- checkAdjustProjection(x)

  color <- vectorColors(x = x,
                        zcol = zcol,
                        colors = color,
                        at = at,
                        na.color = na.color)

  if (npts(x) > getMaxFeatures(x)) {
    leaflet_large(x,
                  map = map,
                  zcol = zcol,
                  color = color,
                  at = at,
                  na.color = na.color,
                  cex = cex,
                  lwd = 1,
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
                  ...)

  } else {

    leaflet_sfc(sf::st_geometry(x),
                map = map,
                zcol = zcol,
                color = color,
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
                ...)

  }
}


  ### large #################################################################
leaflet_large <- function(x,
                          map,
                          zcol,
                          cex,
                          lwd,
                          alpha,
                          alpha.regions,
                          color,
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
                          ...) {

  if (!native.crs) x <- checkAdjustProjection(x)

  m <- initMap(map, map.types, sf::st_crs(x), native.crs)

  m <- addLargeFeatures(m,
                        data = x,
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

  out <- new("mapview", object = list(x), map = m)

  return(out)

}




### MULTIPOINT ############################################################

# leafletMULTIPOINT <- function(x,
#                               map,
#                               cex,
#                               lwd,
#                               alpha,
#                               alpha.regions,
#                               color,
#                               na.color,
#                               map.types,
#                               verbose,
#                               popup,
#                               layer.name,
#                               label,
#                               legend,
#                               legend.opacity,
#                               homebutton,
#                               ...) {
#
#   if(!isAvailableInLeaflet()$lab && verbose) warning(warn)
#
#   m <- initMap(map, map.types, sf::st_crs(x)$proj4string)
#
#   ext <- createExtent(x)
#
#   grp <- layer.name
#   if (is.null(label)) label <- makeLabels(x)
#
#   if (homebutton) m <- addHomeButton(m, ext, layer.name = layer.name)
#
#   color <- mapviewColors(x, colors = color)
#
#
#   if(isAvailableInLeaflet()$lab) {
#     m <- leaflet::addCircleMarkers(m,
#                                    data = x,
#                                    radius = cex,
#                                    weight = lwd,
#                                    opacity = alpha,
#                                    color = color,
#                                    fillOpacity = alpha.regions,
#                                    group = grp,
#                                    label = label,
#                                    popup = popup,
#                                    ...)
#   } else {
#
#     m <- leaflet::addCircleMarkers(m,
#                                    data = x,
#                                    radius = cex,
#                                    weight = lwd,
#                                    opacity = alpha,
#                                    color = color,
#                                    fillOpacity = alpha.regions,
#                                    group = grp,
#                                    popup = popup,
#                                    ...)
#   }
#
#   if(!is.na(sf::st_crs(x)$proj4string)) {
#     crs <- TRUE
#     if (isAvailableInLeaflet()$scl)
#       m <- leaflet::addScaleBar(map = m, position = "bottomleft")
#     m <- addMouseCoordinates(m, style = "detailed")
#   } else {
#     crs <- FALSE
#     m <- addMouseCoordinates(m, style = "basic")
#   }
#
#   m <- mapViewLayersControl(map = m,
#                             map.types = map.types,
#                             names = grp,
#                             hasCRS = crs)
#
#   out <- new('mapview', object = list(x), map = m)
#
#   return(out)
#
# }
