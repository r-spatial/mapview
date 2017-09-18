
addXYGrid = function(map, x) {
  # x = iris_sf

  xrange = mapview:::extendLimits(
    c(sf::st_bbox(x)[["xmin"]],
      sf::st_bbox(x)[["xmax"]])
  )
  yrange = mapview:::extendLimits(
    c(sf::st_bbox(x)[["ymin"]],
      sf::st_bbox(x)[["ymax"]])
  )

  xticks = pretty(xrange)
  xstep = unique(diff(xticks))
  yticks = pretty(yrange)
  ystep = unique(diff(yticks))

  step = min(c(xstep, ystep))

  xticks = c(
    xrange[1] - step * 0.3,
    xticks[2:(length(xticks)-1)],
    xrange[2] + step * 0.3
  )

  yticks = c(
    yrange[1] - step * 0.3,
    yticks[2:(length(yticks)-1)],
    yrange[2] + step * 0.3
  )

  hline = lapply(yticks, function(i) {
    sf::st_linestring(
      rbind(cbind(xticks[1], i), cbind(xticks[length(xticks)], i))
    )
  })

  vline = lapply(xticks, function(i) {
    sf::st_linestring(
      cbind(rbind(i, i), rbind(yticks[1], yticks[length(yticks)]))
    )
  })

  hlines = sf::st_sfc(hline[2:(length(hline)-1)], crs = sf::st_crs(x))
  vlines = sf::st_sfc(vline[2:(length(vline)-1)], crs = sf::st_crs(x))

  hlabs = yticks[2:(length(yticks)-1)]
  vlabs = xticks[2:(length(xticks)-1)]

  out = mapView(
    c(hlines, vlines),
    map = map,
    layer.name = "grid",
    color = "black",
    label = as.character(hlabs),
    lwd = 1,
    alpha = 0.5,
    homebutton = FALSE
  )@map

  out = leaflet::addLabelOnlyMarkers(
    map = out,
    lng = rep(xticks[1], length(hlabs)),
    lat = hlabs,
    label = as.character(hlabs),
    group = "grid",
    labelOptions = leaflet::labelOptions(
      noHide = T,
      direction = "left",
      textOnly = TRUE,
      offset = c(0, -10),
      opacity = 0.5
    )
  )

  out = leaflet::addLabelOnlyMarkers(
    map = out,
    lng = vlabs,
    lat = rep(yticks[1], length(vlabs)),
    label = as.character(vlabs),
    group = "grid",
    labelOptions = leaflet::labelOptions(
      noHide = T,
      direction = "left",
      textOnly = TRUE,
      offset = c(-10, 0),
      opacity = 0.5
    )
  )

  return(out)
}
