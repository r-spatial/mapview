### factor ================================================================
factorPalette <- function(palette,
                          domain,
                          na.color,
                          ...) {
  leaflet::colorFactor(palette = palette,
                       domain = domain,
                       na.color = na.color)
}

factorLegend <- function(map,
                         position,
                         values,
                         colors,
                         na.color,
                         layer.name,
                         ...) {

  pltt = unique(
    zcolColors(
      x = values[!is.na(values)]
      , colors = colors
      , na.color = na.color
      , return.sorted = ifelse(is.function(colors), TRUE, FALSE)
    )
  )

  if (length(levels(values)) == length(colors)) {
    pltt = zcolColors(
      x = values
      , colors = colors
      , na.color = na.color
      , return.sorted = ifelse(is.function(colors), TRUE, FALSE)
    )
  }

  pal <- factorPalette(
    palette = pltt
    , domain = values
    , na.color = na.color
  )

  mvAddLegend(isAvailableInLeaflet()$leggrp,
              layer.name,
              map = map,
              position = position,
              pal = pal,
              values = values,
              opacity = mapviewGetOption("legend.opacity"),
              title = ifelse(length(values) > 1, layer.name, ""),
              ...)

}

### character =============================================================
characterLegend <- function(map,
                            position,
                            values,
                            colors,
                            na.color,
                            layer.name) {
  vals = values

  if ( is.function(colors)) {
    vals = unique(values[!is.na(values)])
  }

  if (inherits(values, "POSIXt")) {
    vals = format(values)
  }

  pal <- factorPalette(
    palette = zcolColors(
      x = vals
      , colors = colors
      , na.color = na.color
      , return.sorted = ifelse(is.function(colors), TRUE, FALSE)
    )
    , domain = vals
    , na.color = na.color
  )
  mvAddLegend(isAvailableInLeaflet()$leggrp,
              layer.name,
              map = map,
              position = position,
              pal = pal,
              values = vals,
              opacity = mapviewGetOption("legend.opacity"),
              title = ifelse(length(values) > 1, layer.name, ""))
}


### numeric ===============================================================
numericPalette <- function(palette,
                           domain,
                           na.color,
                           ...) {
  leaflet::colorNumeric(palette = palette,
                        domain = domain,
                        na.color = na.color)
}

binPalette <- function(palette,
                       domain,
                       bins,
                       na.color,
                       ...) {
  leaflet::colorBin(palette = palette,
                    domain = domain,
                    bins = bins,
                    na.color = na.color)
}

numericLegend <- function(map,
                          position,
                          values,
                          colors,
                          at,
                          na.color,
                          layer.name,
                          ...) {

  n_unique <- ifelse(is.null(at), length(unique(values)), length(at))

  if (inherits(values, "POSIXt")) {
    labForm = function(type, ...) {
      switch(type, numeric = (function(cuts) {
        format(as.POSIXct(cuts))
      })(...))
    }
    pal <- numericPalette(palette = colors(n_unique),
                          domain = as.numeric(values),
                          na.color = na.color,
                          ...)
    mvAddLegend(isAvailableInLeaflet()$leggrp,
                layer.name,
                map = map,
                position = position,
                pal = pal,
                values = as.numeric(values),
                labFormat = labForm,
                opacity = mapviewGetOption("legend.opacity"),
                title = ifelse(length(values) > 1, layer.name, ""),
                ...)
  } else {

    if (is.null(at)) {
      atc <- lattice::do.breaks(
        extendLimits(
          range(values, na.rm = TRUE)
        )
        , length(unique(values))
      )
    } else atc <- at

    ## in case people complain, add the <= 1 part as an option!!
    if (is.null(at) & n_unique <= 1 & all(unique(values) %% 1 == 0, na.rm = TRUE)) {
      factorLegend(map = map,
                   values = as.factor(unique(values)),
                   colors = colors,
                   position = position,
                   layer.name = layer.name,
                   na.color = na.color)
    } else if (!(is.null(at))) {
      if (anyNA(values)) values <- c(atc, NA) else values <- atc
      pal <- binPalette(palette = colors(n_unique),
                        domain = atc,
                        bins = atc,
                        na.color = na.color,
                        ...)
      mvAddLegend(isAvailableInLeaflet()$leggrp,
                  layer.name,
                  map = map,
                  position = position,
                  pal = pal,
                  values = values,
                  opacity = mapviewGetOption("legend.opacity"),
                  title = ifelse(length(values) > 1, layer.name, ""),
                  ...)
    } else if (is.null(at)) {
      pal <- numericPalette(palette = colors(n_unique),
                            domain = values,
                            na.color = na.color,
                            ...)
      mvAddLegend(isAvailableInLeaflet()$leggrp,
                  layer.name,
                  map = map,
                  position = position,
                  pal = pal,
                  values = values,
                  opacity = mapviewGetOption("legend.opacity"),
                  title = ifelse(length(values) > 1, layer.name, ""),
                  ...)
    }
  }

}


mvAddLegend = function(grp_avail = isAvailableInLeaflet()$leggrp,
                       layer.name, ...) {
  args = list(...)
  leg_args = match.arg(
    names(args)
    , names(as.list(args(leaflet::addLegend)))
    , several.ok = TRUE
  )

  if (grp_avail) {
    do.call(leaflet::addLegend, c(args[leg_args], list(group = layer.name)))
  } else {
    do.call(leaflet::addLegend, c(args[leg_args]))
  }
}

#' @importFrom stats na.omit
### mapviewLegend =========================================================
mapviewLegend <- function(values,
                          colors,
                          at,
                          na.color,
                          layer.name,
                          position = mapviewGetOption("legend.pos"),
                          ...) {

  # values = as.vector(values)
  ## factor
  ## if character convert to factor
  if (inherits(values, "character")) {
    values = as.factor(values)
  }

  ## numeric
  ## if interger convert to numeric
  if (inherits(values, "integer")) {
    values = as.numeric(values)
  }

  if (length(colors) > 1) {

    if (inherits(values, "factor")) {
      if (length(values) == length(colors)) {
        values = droplevels(values)
        values = unique(values)
        # values = factor(unique(droplevels(values)), levels = unique(droplevels(values)))
        colors = unique(colors)[as.numeric(values)]
      } else if (length(levels(values)) >= length(unique(colors))) {
        values = unique(values)
        colors = as.vector(stats::na.omit(colors[levels(values) %in% values]))
        values = droplevels(values)
      } else {
        values = unique(droplevels(values))
        colors = unique(colors)
      }

      if (length(colors) > length(values)) {
        colors = colors[1:length(values)]
      }

      if (length(colors) < length(values)) {
        colors = rep_len(colors, length(values))
      }

    }

    if (inherits(values, "numeric")) {
      values = unique(values)
      colors = unique(colors)

      if (length(colors) > length(values)) {
        colors = grDevices::colorRampPalette(colors[1:length(values)])
      } else if (length(colors) < length(values)) {
        colors = grDevices::colorRampPalette(colors)
      }
      if (length(colors) == length(values)) {
        colors = colors #colors[order(values)]
        values = as.factor(values)
      }
    }


  }

  function(map) {

    # if values inherits from more than one class, select
    value.class = class(values)[length(class(values))]

    switch(value.class,
           factor = factorLegend(map,
                                 position = position,
                                 values = values, #levels(values),
                                 colors = colors,
                                 na.color = na.color,
                                 layer.name = layer.name,
                                 ...),
           character = characterLegend(map,
                                       position = position,
                                       values = values,
                                       colors = colors,
                                       na.color = na.color,
                                       layer.name = layer.name,
                                       ...),
           numeric = numericLegend(map,
                                   position = position,
                                   values = values,
                                   colors = colors,
                                   at = at,
                                   na.color = na.color,
                                   layer.name = layer.name,
                                   ...),
           units = numericLegend(map,
                                 position = position,
                                 values = as.numeric(values),
                                 colors = colors,
                                 at = at,
                                 na.color = na.color,
                                 layer.name = layer.name,
                                 ...),
           integer = numericLegend(map,
                                   position = position,
                                   values = values,
                                   colors = colors,
                                   at = at,
                                   na.color = na.color,
                                   layer.name = layer.name,
                                   ...),
           POSIXt = numericLegend(map,
                                  position = position,
                                  values = values,
                                  colors = colors,
                                  at = at,
                                  na.color = na.color,
                                  layer.name = layer.name,
                                  ...)
           )
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
    ## in case people complain, add this as an option!!
    # if (length(at) > 11) {
    pal2 <- leaflet::colorNumeric(palette = col.regions,
                                  domain = at,
                                  na.color = col2Hex(na.color))
    # } else {
    #   pal2 <- leaflet::colorBin(palette = col.regions,
    #                             bins = at, #length(at),
    #                             domain = at,
    #                             na.color = col2Hex(na.color))
    # }

  }

  m <- leaflet::addLegend(map = map,
                          position = mapviewGetOption("legend.pos"),
                          values = leg_vals,
                          pal = pal2,
                          opacity = mapviewGetOption("legend.opacity"),
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

  is.fact <- if (inherits(x, "stars"))
		is.factor(x[[1]])
  	else
		is.factor(x)

  if (is.fact) {
    vals <- as.character(x[])
  } else if (inherits(x, "stars")) {
	stopifnot (length(dim(x)) <= 3)
	if (length(dim(x)) == 3)
    	vals = as.vector(x[, , , 1][[1]][])
	else
    	vals = as.vector(x[[1]])
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
                                 # levels = as.character(levels(x)[[1]][, 2]),
                                 na.color = col2Hex(na.color))
  } else {
    ## in case people complain, add this as an option!!
    # if (length(at) > 11) {
    pal2 <- leaflet::colorNumeric(palette = col.regions,
                                  domain = at,
                                  na.color = col2Hex(na.color))
    # } else {
    #   pal2 <- leaflet::colorBin(palette = col.regions,
    #                             bins = at, #length(at),
    #                             domain = at,
    #                             na.color = col2Hex(na.color))
    # }

  }

  if (isAvailableInLeaflet()$leggrp) {
    m <- leaflet::addLegend(map = map,
                            position = mapviewGetOption("legend.pos"),
                            values = leg_vals,
                            group = group,
                            pal = pal2,
                            opacity = mapviewGetOption("legend.opacity"),
                            labFormat = labelFormat(big.mark = ""),
                            title = title)
  } else {
    m <- leaflet::addLegend(map = map,
                            position = mapviewGetOption("legend.pos"),
                            values = leg_vals,
                            pal = pal2,
                            opacity = mapviewGetOption("legend.opacity"),
                            labFormat = labelFormat(big.mark = ""),
                            title = title)
  }

  return(m)

}


## legend for plainview, slideview, cubeview ==============================
rasterLegend <- function(key) {
  draw.colorkey(key = key, draw = TRUE)
}
