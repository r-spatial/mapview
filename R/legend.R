### factor ================================================================
factorPalette <- function(palette,
                          domain,
                          na.color,
                          ...) {
  leaflet::colorFactor(palette = palette,
                       domain = domain,
                       na.color = na.color,
                       ...)
}

factorLegend <- function(map,
                         values,
                         colors,
                         na.color) {
  pal <- factorPalette(palette = colors(length(levels(values))),
                       domain = values,
                       na.color = na.color)
  leaflet::addLegend(map = map,
                     pal = pal,
                     values = values,
                     opacity = 1)
}

### character =============================================================
characterLegend <- function(map,
                            values,
                            colors,
                            na.color) {
  pal <- factorPalette(palette = colors(length(unique(values))),
                       domain = values,
                       na.color = na.color)
  leaflet::addLegend(map = map,
                     pal = pal,
                     values = values,
                     opacity = 1)
}


### numeric ===============================================================
numericPalette <- function(palette,
                           domain,
                           na.color,
                           ...) {
  leaflet::colorNumeric(palette = palette,
                        domain = domain,
                        na.color = na.color,
                        ...)
}

binPalette <- function(palette,
                       domain,
                       bins,
                       na.color,
                       ...) {
  leaflet::colorBin(palette = palette,
                    domain = domain,
                    bins = bins,
                    na.color = na.color,
                    ...)
}

numericLegend <- function(map,
                          values,
                          colors,
                          at,
                          na.color) {
  n_unique <- ifelse(is.null(at), length(unique(values)), length(at))
  if (is.null(at)) at <- unique(values)
  if (n_unique <= 11) {
    if (anyNA(values)) leg_vals <- c(at, NA) else leg_vals <- at
    pal <- binPalette(palette = colors(n_unique),
                      domain = at,
                      bins = at,
                      na.color = na.color)
    leaflet::addLegend(map = map,
                       pal = pal,
                       values = leg_vals,
                       opacity = 1)
  } else {
    pal <- numericPalette(palette = colors(n_unique),
                          domain = values,
                          na.color = na.color)
    leaflet::addLegend(map = map,
                       pal = pal,
                       values = values,
                       opacity = 1)
  }
}


### mapviewLegend =========================================================
mapviewLegend <- function(values,
                          colors,
                          at,
                          na.color) {

  function(map) {
    switch(class(values),
           factor = factorLegend(map,
                                 values = values,
                                 colors = colors,
                                 na.color = na.color),
           character = characterLegend(map,
                                       values = values,
                                       colors = colors,
                                       na.color = na.color),
           numeric = numericLegend(map,
                                   values = values,
                                   colors = colors,
                                   at = at,
                                   na.color = na.color),
           integer = numericLegend(map,
                                   values = values,
                                   colors = colors,
                                   at = at,
                                   na.color = na.color))
  }
}



### legacy ================================================================
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
