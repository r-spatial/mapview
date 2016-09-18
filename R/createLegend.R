
addVectorLegend <- function(x,
                            map,
                            zcol,
                            at,
                            col.regions,
                            na.color) {

  is.fact <- is.factor(x@data[, zcol])

  if (is.fact) {
    vals <- as.character(x@data[, zcol])
  } else {
    vals <- x@data[, zcol] # orig values needed for legend creation later on
  }

  if (is.null(at)) {
    if (is.fact) {
      at <- vals
    } else {
      at <- lattice::do.breaks(range(vals,
                                     na.rm = TRUE),
                               length(vals))
    }
  }

  if (any(is.na(vals))) {
    leg_vals <- c(at, NA)
  } else leg_vals <- at

  if (is.function(col.regions)) col.regions <- col.regions(length(at))

  if (is.fact) {
    pal2 <- leaflet::colorFactor(palette = col.regions,
                                 domain = at,
                                 na.color = col2Hex(na.color))
  } else {
    if (length(at) > 11) {
      pal2 <- leaflet::colorNumeric(palette = col.regions,
                                    domain = at,
                                    na.color = col2Hex(na.color))
    } else {
      pal2 <- leaflet::colorBin(palette = col.regions,
                                bins = at, #length(at),
                                domain = at,
                                na.color = col2Hex(na.color))
    }

  }

  m <- leaflet::addLegend(map = map,
                          position = mapviewGetOption("legend.pos"),
                          values = leg_vals,
                          pal = pal2,
                          opacity = 1,
                          labFormat = labelFormat(big.mark = ""),
                          title = zcol)

  return(m)

}



### raster
addRasterLegend <- function(x,
                            map,
                            title,
                            at,
                            col.regions,
                            na.color) {

  is.fact <- is.factor(x)

  if (is.fact) {
    vals <- as.character(x[])
  } else {
    vals <- x[] # orig values needed for legend creation later on
  }

  if (is.null(at)) {
    if (is.fact) {
      at <- vals
    } else {
      at <- lattice::do.breaks(range(vals,
                                     na.rm = TRUE),
                               length(vals))
    }
  }

  if (any(is.na(vals))) {
    leg_vals <- c(at, NA)
  } else leg_vals <- at

  if (is.function(col.regions)) col.regions <- col.regions(length(at))

  if (is.fact) {
    pal2 <- leaflet::colorFactor(palette = col.regions,
                                 domain = at,
                                 na.color = col2Hex(na.color))
  } else {
    if (length(at) > 11) {
      pal2 <- leaflet::colorNumeric(palette = col.regions,
                                    domain = at,
                                    na.color = col2Hex(na.color))
    } else {
      pal2 <- leaflet::colorBin(palette = col.regions,
                                bins = at, #length(at),
                                domain = at,
                                na.color = col2Hex(na.color))
    }

  }

  m <- leaflet::addLegend(map = map,
                          position = mapviewGetOption("legend.pos"),
                          values = leg_vals,
                          pal = pal2,
                          opacity = 1,
                          labFormat = labelFormat(big.mark = ""),
                          title = title)

  return(m)

}
