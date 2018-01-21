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
                         position,
                         values,
                         colors,
                         na.color,
                         layer.name) {
  pal <- factorPalette(palette = zcolColors(x = levels(values),
                                            colors = colors,
                                            na.color = na.color,
                                            return.sorted = TRUE),
                       domain = values,
                       na.color = na.color)
  mvAddLegend(isAvailableInLeaflet()$leggrp,
              layer.name,
              map = map,
              position = position,
              pal = pal,
              values = values,
              opacity = 1,
              title = layer.name)

}

### character =============================================================
characterLegend <- function(map,
                            position,
                            values,
                            colors,
                            na.color,
                            layer.name) {
  vals = unique(values[!is.na(values)])
  pal <- factorPalette(palette = zcolColors(x = vals,
                                            colors = colors,
                                            na.color = na.color,
                                            return.sorted = TRUE),
                       domain = values,
                       na.color = na.color)
  mvAddLegend(isAvailableInLeaflet()$leggrp,
              layer.name,
              map = map,
              position = position,
              pal = pal,
              values = values,
              opacity = 1,
              title = layer.name)
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
                          position,
                          values,
                          colors,
                          at,
                          na.color,
                          layer.name) {
  n_unique <- ifelse(is.null(at), length(unique(values)), length(at))
  if (is.null(at)) {
    atc <- lattice::do.breaks(range(values, na.rm = TRUE),
                              length(unique(values)))
  } else atc <- at

  if (is.null(at) & n_unique <= 11 & all(unique(values) %% 1 == 0, na.rm = TRUE)) {
    factorLegend(map = map,
                 values = as.factor(unique(values)),
                 colors = colors,
                 position = position,
                 layer.name = layer.name,
                 na.color = na.color)
  } else if (n_unique <= 11) {
    if (anyNA(values)) values <- c(atc, NA) else values <- atc
    pal <- binPalette(palette = colors(n_unique),
                      domain = atc,
                      bins = atc,
                      na.color = na.color)
    mvAddLegend(isAvailableInLeaflet()$leggrp,
                layer.name,
                map = map,
                position = position,
                pal = pal,
                values = values,
                opacity = 1,
                title = layer.name)

  } else {
    pal <- numericPalette(palette = colors(n_unique),
                          domain = values,
                          na.color = na.color)
    mvAddLegend(isAvailableInLeaflet()$leggrp,
                layer.name,
                map = map,
                position = position,
                pal = pal,
                values = values,
                opacity = 1,
                title = layer.name)

  }
}


mvAddLegend = function(grp_avail = isAvailableInLeaflet()$leggrp,
                       layer.name, ...) {
  if (grp_avail) leaflet::addLegend(..., group = layer.name) else
    leaflet::addLegend(...)
}


### mapviewLegend =========================================================
mapviewLegend <- function(values,
                          colors,
                          at,
                          na.color,
                          layer.name,
                          position = mapviewGetOption("legend.pos")) {

  if (!is.function(colors) & inherits(colors, "character")) {
    colors <- grDevices::colorRampPalette(colors)
  }

  function(map) {
    switch(class(values),
           factor = factorLegend(map,
                                 position = position,
                                 values = values,
                                 colors = colors,
                                 na.color = na.color,
                                 layer.name = layer.name),
           character = characterLegend(map,
                                       position = position,
                                       values = values,
                                       colors = colors,
                                       na.color = na.color,
                                       layer.name = layer.name),
           numeric = numericLegend(map,
                                   position = position,
                                   values = values,
                                   colors = colors,
                                   at = at,
                                   na.color = na.color,
                                   layer.name = layer.name),
           units = numericLegend(map,
                                 position = position,
                                 values = as.numeric(values),
                                 colors = colors,
                                 at = at,
                                 na.color = na.color,
                                 layer.name = layer.name),
           integer = numericLegend(map,
                                   position = position,
                                   values = values,
                                   colors = colors,
                                   at = at,
                                   na.color = na.color,
                                   layer.name = layer.name))
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
                            group,
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

  if (isAvailableInLeaflet()$leggrp) {
    m <- leaflet::addLegend(map = map,
                            position = mapviewGetOption("legend.pos"),
                            values = leg_vals,
                            group = group,
                            pal = pal2,
                            opacity = 1,
                            labFormat = labelFormat(big.mark = ""),
                            title = title)
  } else {
    m <- leaflet::addLegend(map = map,
                            position = mapviewGetOption("legend.pos"),
                            values = leg_vals,
                            pal = pal2,
                            opacity = 1,
                            labFormat = labelFormat(big.mark = ""),
                            title = title)
  }

  return(m)

}


## legend for plainview, slideview, cubeview ==============================
rasterLegend <- function(col, at, ...) {
  draw.colorkey(key = list(col = col,
                           at = at,
                           ...), draw = TRUE)
}
