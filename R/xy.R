# make interactive scatter plots

xyView = function(x, y, data, type = "p", grid = TRUE, aspect = 1, label, ...) {

  if (!missing(data)) {
    # nm = "data" #deparse(substitute(data))
    data[[y]] = data[[y]] * aspect
    data = sf::st_as_sf(data, coords = c(x, y), remove = FALSE)
  } else if (!missing(x) & missing(y)) {
    y = x * aspect
    x = seq_along(y)
    data = sf::st_as_sf(
      data.frame(x = x, y = y),
      coords = c("x", "y"),
      remove = FALSE
    )
    # layer.name = "data"
    # stop("need both x and y if data is missing")
  } else {
    data = sf::st_as_sf(
      data.frame(x = x, y = y * aspect),
      coords = c("x", "y"),
      remove = FALSE
    )
    # nm = "data"
  }

  if (type == "l") {
    data = sf::st_sfc(
      sf::st_cast(
        sf::st_combine(
          data
        ),
        to = "LINESTRING"
      )[[1]]
    )
  }

  xlab = sf::st_coordinates(data)[, "X"]
  ylab = sf::st_coordinates(data)[, "Y"] / aspect
  # labs = lapply(seq(xlab), function(i) paste(xlab[i], ylab[i], sep = ", "))

  if (grid) out = xyGrid(data, aspect) else out = NULL
  out = mapView(
    data,
    map = out,
    highlight = NULL,
    label = label,
    ...
  )
  return(out)
}


xyGrid = function(x, aspect = 1) {
  # x = iris_sf

  xrange = extendLimits(
    c(sf::st_bbox(x)[["xmin"]],
      sf::st_bbox(x)[["xmax"]])
  )
  yrange = extendLimits(
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

  hlabs = yticks[2:(length(yticks)-1)] * (1 / aspect)
  vlabs = xticks[2:(length(xticks)-1)]

  hout = mapView(
    hlines,
    layer.name = "y_grid",
    color = "black",
    label = as.character(hlabs),
    lwd = 1,
    alpha = 0.5,
    homebutton = FALSE
  )@map

  vout = mapView(
    vlines,
    map = hout,
    layer.name = "x_grid",
    color = "black",
    label = as.character(vlabs),
    lwd = 1,
    alpha = 0.5,
    homebutton = FALSE
  )@map

  out = leaflet::addLabelOnlyMarkers(
    map = vout,
    lng = rep(xticks[1], length(hlabs)),
    lat = hlabs * aspect,
    label = as.character(hlabs),
    group = "y_grid",
    labelOptions = leaflet::labelOptions(
      noHide = TRUE,
      direction = "left",
      textOnly = TRUE,
      offset = c(10, 25),
      opacity = 0.5
    )
  )

  out = leaflet::addLabelOnlyMarkers(
    map = out,
    lng = vlabs,
    lat = rep(yticks[1], length(vlabs)),
    label = as.character(vlabs),
    group = "x_grid",
    labelOptions = leaflet::labelOptions(
      noHide = TRUE,
      direction = "bottom",
      textOnly = TRUE,
      offset = c(0, -10),
      opacity = 0.5
    )
  )

  return(out)
}
