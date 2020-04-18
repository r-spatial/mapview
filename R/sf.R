### sf ####################################################################
leaflet_sf <- function(x,
                       map,
                       pane,
                       canvas,
                       viewer.suppress,
                       zcol,
                       cex,
                       lwd,
                       alpha,
                       alpha.regions,
                       color,
                       col.regions,
                       at,
                       na.color,
                       na.alpha,
                       map.types,
                       verbose,
                       popup,
                       layer.name,
                       label,
                       legend,
                       legend.opacity,
                       homebutton,
                       native.crs,
                       highlight,
                       maxpoints,
                       ...) {

  if (inherits(sf::st_geometry(x), "sfc_MULTIPOINT"))
    x = suppressWarnings(sf::st_cast(x, "POINT"))

  if (is.null(layer.name)) layer.name = makeLayerName(x, zcol)
  cex <- circleRadius(x, cex, ...)
  if (is.null(zcol) & ncol(sf2DataFrame(x, drop_sf_column = TRUE)) == 1) {
    zcol = colnames(sf2DataFrame(x, drop_sf_column = TRUE))[1]
    label = makeLabels(x, zcol)
  }
  if (!is.null(zcol)) {
    if (inherits(x[[zcol]], "logical")) x[[zcol]] = as.character(x[[zcol]])
    if (inherits(x[[zcol]], "character")) x[[zcol]] = as.factor(x[[zcol]])
    ## colors ---
    if (length(unique(x[[zcol]])) == 1) {
      color = ifelse(is.function(color), standardColor(x), color)
      col.regions = ifelse(is.function(col.regions), standardColRegions(x), col.regions)
    }
  }

  ## legend ----
  if (legend) {
    # if (is.null(zcol)) zcol = 1
    if (is.null(zcol)) vals = layer.name else vals = x[[zcol]]
    if (length(unique(vals)) == 1) {
      color = ifelse(is.function(color), standardColor(x), color)
      col.regions = ifelse(is.function(col.regions), standardColRegions(x), col.regions)
    }
    if (getGeometryType(x) == "ln") leg_clrs <- color else leg_clrs <- col.regions
    legend <- mapviewLegend(values = vals,
                            colors = leg_clrs,
                            at = at,
                            na.color = col2Hex(na.color),
                            layer.name = layer.name)
  }

  clrs <- vectorColors(x = x,
                       zcol = zcol,
                       colors = color,
                       at = at,
                       na.color = na.color)
  clrs.regions <- vectorColRegions(x = x,
                                   zcol = zcol,
                                   col.regions = col.regions,
                                   at = at,
                                   na.color = na.color)
  if (!is.null(zcol) & !is.null(na.alpha)) {
    na.alpha = ifelse(na.alpha == 0, 0.001, na.alpha)
    if (length(alpha) != nrow(x)) alpha = rep(alpha, nrow(x))
    alpha[is.na(x[[zcol]])] = na.alpha #[is.na(x[[zcol]])]
    if (length(alpha.regions) != nrow(x)) alpha.regions = rep(alpha.regions, nrow(x))
    alpha.regions[is.na(x[[zcol]])] = na.alpha #[is.na(x[[zcol]])]
  }

  ## if gl we need to cast MULTI* and redo the popup if it's a popupTable
  if ("gl" %in% names(list(...)) && isTRUE(list(...)$gl)) {
    if (inherits(sf::st_geometry(x), "sfc_MULTIPOLYGON")) {
      x = suppressWarnings(sf::st_cast(x, "POLYGON"))
      if (!(is.null(attributes(popup))) && names(attributes(popup)) == "popup") {
        popup = leafpop::popupTable(x)
      }
    }
    if (inherits(sf::st_geometry(x), "sfc_MULTILINESTRING")) {
      x = suppressWarnings(sf::st_cast(x, "LINESTRING"))
      if (!(is.null(attributes(popup))) && names(attributes(popup)) == "popup") {
        popup = leafpop::popupTable(x)
      }
    }
    if (inherits(sf::st_geometry(x), "sfc_MULTIPOINT")) {
      x = suppressWarnings(sf::st_cast(x, "POINT"))
      if (!(is.null(attributes(popup))) && names(attributes(popup)) == "popup") {
        popup = leafpop::popupTable(x)
      }
    }
  }

  leaflet_sfc(sf::st_geometry(x),
              map = map,
              pane = pane,
              zcol = zcol,
              color = clrs,
              col.regions = clrs.regions,
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
              highlight = highlight,
              maxpoints = maxpoints,
              attributes = sf2DataFrame(x, drop_sf_column = TRUE),
              canvas = canvas,
              viewer.suppress = viewer.suppress,
              ...)

}


leafgl_sf = function(x,
                     map,
                     zcol,
                     color,
                     col.regions,
                     at,
                     na.color,
                     cex,
                     lwd,
                     alpha,
                     alpha.regions,
                     na.alpha,
                     map.types,
                     verbose,
                     popup,
                     layer.name,
                     label,
                     legend,
                     legend.opacity,
                     homebutton,
                     native.crs,
                     highlight,
                     maxpoints,
                     viewer.suppress,
                     ...) {

  if (inherits(sf::st_geometry(x), "sfc_MULTIPOLYGON")) {
    x = suppressWarnings(sf::st_cast(x, "POLYGON"))
    if (!(is.null(attributes(popup))) && names(attributes(popup)) == "popup") {
      popup = leafpop::popupTable(x)
    }
  }
  if (inherits(sf::st_geometry(x), "sfc_MULTILINESTRING")) {
    x = suppressWarnings(sf::st_cast(x, "LINESTRING"))
    if (!(is.null(attributes(popup))) && names(attributes(popup)) == "popup") {
      popup = leafpop::popupTable(x)
    }
  }
  if (inherits(sf::st_geometry(x), "sfc_MULTIPOINT")) {
    x = suppressWarnings(sf::st_cast(x, "POINT"))
    if (!(is.null(attributes(popup))) && names(attributes(popup)) == "popup") {
      popup = leafpop::popupTable(x)
    }
  }

  if (is.null(layer.name)) layer.name = makeLayerName(x, zcol)
  cex <- circleRadius(x, cex, ...)
  if (is.null(zcol) & ncol(sf2DataFrame(x, drop_sf_column = TRUE)) == 1) {
    zcol = colnames(sf2DataFrame(x, drop_sf_column = TRUE))[1]
    label = makeLabels(x, zcol)
  }
  if (!is.null(zcol)) {
    if (inherits(x[[zcol]], "logical")) x[[zcol]] = as.character(x[[zcol]])
    if (inherits(x[[zcol]], "character")) x[[zcol]] = as.factor(x[[zcol]])
    ## colors ---
    if (length(unique(x[[zcol]])) == 1) {
      color = ifelse(is.function(color), standardColor(x), color)
      col.regions = ifelse(is.function(col.regions), standardColRegions(x), col.regions)
    }
  }

  ## legend ----
  if (legend) {
    # if (is.null(zcol)) zcol = 1
    if (is.null(zcol)) vals = layer.name else vals = x[[zcol]]
    if (length(unique(vals)) == 1) {
      color = ifelse(is.function(color), standardColor(x), color)
      col.regions = ifelse(is.function(col.regions), standardColRegions(x), col.regions)
    }
    if (getGeometryType(x) == "ln") leg_clrs <- color else leg_clrs <- col.regions
    legend <- mapviewLegend(values = vals,
                            colors = leg_clrs,
                            at = at,
                            na.color = col2Hex(na.color),
                            layer.name = layer.name)
  }

  clrs <- vectorColors(x = x,
                       zcol = zcol,
                       colors = color,
                       at = at,
                       na.color = na.color)
  clrs.regions <- vectorColRegions(x = x,
                                   zcol = zcol,
                                   col.regions = col.regions,
                                   at = at,
                                   na.color = na.color)

  # ## legend ----
  # if (legend) {
  #   # if (is.null(zcol)) zcol = 1
  #   if (is.null(zcol)) vals = layer.name else vals = x[[zcol]]
  #   if (length(unique(vals)) == 1) {
  #     color = ifelse(is.function(color), standardColor(x), color)
  #     col.regions = ifelse(is.function(col.regions), standardColRegions(x), col.regions)
  #   }
  #   if (getGeometryType(x) == "ln") leg_clrs <- color else leg_clrs <- col.regions
  #   legend <- mapviewLegend(values = vals,
  #                           colors = leg_clrs,
  #                           at = at,
  #                           na.color = col2Hex(na.color),
  #                           layer.name = layer.name)
  # }

  if (!native.crs) x <- checkAdjustProjection(x)
  if (is.na(sf::st_crs(x)$proj4string)) native.crs <- TRUE

  if (is.null(map.types)) {
    if (getGeometryType(x) %in% c("pl", "pt")) {
      if (is.function(col.regions)) col.regions <- standardColRegions(x)
      map.types <- basemaps(col.regions)
    } else {
      if (is.function(color)) color <- standardColor(x)
      map.types <- basemaps(color)
    }
  }

  # if (!is.null(zcol)) {
  #   if (!is.null(color)) {
  #     color = ifelse(getGeometryType(x) %in% c("pl", "pt"), standardColor(x), zcol)
  #   }
  #   col.regions = ifelse(getGeometryType(x) %in% c("pl", "pt"), zcol, standardColor(x))
  # } else {
  #   if (!is.null(color)) {
  #     color = ifelse(is.function(color), standardColor(x), color)
  #   }
  #   col.regions = ifelse(is.function(col.regions), standardColRegions(x), col.regions)
  # }

  label = makeLabels(x, zcol)
  x$label = label

  m <- initMap(
    map,
    map.types,
    sf::st_crs(x),
    native.crs,
    viewer.suppress = viewer.suppress,
    ...
  )

  m <- leafem::addFeatures(
    m
    , data = x
    , radius = cex * 2
    , weight = lwd / 2
    , opacity = alpha
    , fillOpacity = alpha.regions
    , color = clrs
    , fillColor = clrs.regions
    , legend = legend
    , popup = popup
    , group = layer.name
    , gl = TRUE
    , ...
  )

  ## if polygons, also plot polygon borders
  if (inherits(sf::st_geometry(x), "sfc_POLYGON") & lwd > 0) {
    m = leafem::addFeatures(
      m
      , data = suppressWarnings(sf::st_cast(x, "LINESTRING"))
      , weight = lwd / 2
      , opacity = alpha
      , color = clrs
      , legend = FALSE
      , popup = NULL
      , group = layer.name
      , gl = TRUE
      , ...
    )
  }

  if (!is.null(map)) m = updateOverlayGroups(m, layer.name)
  sclbrpos = getCallEntryFromMap(m, "addScaleBar")
  if (length(sclbrpos) > 0 | native.crs) scalebar = FALSE else scalebar = TRUE

  funs <- list(if (scalebar) leaflet::addScaleBar,
               if (homebutton) leafem::addHomeButton,
               if (is.null(map)) mapViewLayersControl,
               leafem::addMouseCoordinates)
  funs <- funs[!sapply(funs, is.null)]

  args <- list(if (scalebar) list(position = "bottomleft"),
               if (homebutton) list(ext = createExtent(x),
                                    group = layer.name),
               if (is.null(map)) list(map.types = map.types,
                                      names = layer.name,
                                      native.crs = native.crs),
               list(style = "detailed",
                    epsg = sf::st_crs(x)$epsg,
                    proj4string = sf::st_crs(x)$proj4string,
                    native.crs = native.crs))
  args <- args[!sapply(args, is.null)]

  m <- decorateMap(map = m,
                   funs = funs,
                   args = args)

  try(
    if (attributes(popup)$popup == "leafpop") {
      m$dependencies <- c(m$dependencies, popupLayoutDependencies())
    }
    , silent = TRUE
  )

  if (is.function(legend)) m <- legend(m)
  m = removeDuplicatedMapDependencies(m)
  out <- new("mapview", object = list(x), map = m)

  return(out)

}


mapdeck_sf = function(x,
                      map,
                      zcol,
                      color,
                      col.regions,
                      at,
                      na.color,
                      cex,
                      lwd,
                      alpha,
                      alpha.regions,
                      na.alpha,
                      map.types,
                      verbose,
                      popup,
                      layer.name,
                      label,
                      legend,
                      legend.opacity,
                      homebutton,
                      native.crs,
                      highlight,
                      maxpoints,
                      viewer.suppress,
                      ...) {

  ## if x is polygon and elevation is provided -> set color and lwd to NULL to
  ## enable extrusion
  if ("elevation" %in% names(list(...)) & getGeometryType(x) == "pl") {
    color = NULL
    lwd = NULL
  }

  if (is.null(layer.name)) layer.name = makeLayerName(x, zcol)
  cex <- circleRadius(x, cex, ...)
  if (is.null(zcol) & ncol(sf2DataFrame(x, drop_sf_column = TRUE)) == 1) {
    zcol = colnames(sf2DataFrame(x, drop_sf_column = TRUE))[1]
  }
  if (!is.null(zcol)) {
    if (inherits(x[[zcol]], "logical")) x[[zcol]] = as.character(x[[zcol]])
    if (length(unique(x[[zcol]])) == 1) {
      color = ifelse(is.function(color), standardColor(x), color)
      col.regions = ifelse(is.function(col.regions), standardColRegions(x), col.regions)
    }
  }

  # x = sf::st_geometry(x)
  #
  # if (!is.null(names(x))) names(x) = NULL
  # if (is_literally_false(highlight)) highlight = NULL
  # if (is_literally_false(popup)) popup = NULL
  # if (inherits(x, "XY")) x = sf::st_sfc(x)
  if (!native.crs) x <- checkAdjustProjection(x)
  if (is.na(sf::st_crs(x)$proj4string)) native.crs <- TRUE

  if (is.null(map.types)) {
    if (getGeometryType(x) %in% c("pl", "pt")) {
      if (is.function(col.regions)) col.regions <- standardColRegions(x)
      map.types <- as.vector(stats::na.omit(basemaps(col.regions)))
    } else {
      if (is.function(color)) color <- standardColor(x)
      map.types <- as.vector(stats::na.omit(basemaps(color)))
    }
  }

  # if (is.function(color)) color = color(nrow(x))
  # if (is.function(col.regions)) col.regions = col.regions(nrow(x))
  if (!is.null(zcol)) {
    if (!is.null(color)) {
      color = ifelse(getGeometryType(x) %in% c("pl", "pt"), standardColor(x), zcol)
    }
    col.regions = ifelse(getGeometryType(x) %in% c("pl", "pt"), zcol, standardColor(x))
  } else {
    if (!is.null(color)) {
      color = ifelse(is.function(color), standardColor(x), color)
    }
    col.regions = ifelse(is.function(col.regions), standardColRegions(x), col.regions)
  }

  label = makeLabels(x, zcol)
  x$label = label

  m <- initMap(
    map,
    map.types,
    sf::st_crs(x),
    native.crs,
    viewer.suppress = viewer.suppress,
    ...
  )

  if (!is.null(lwd)) {
    lwd = ifelse(getGeometryType(x) == "pl", lwd * 100, lwd)
  }

  m <- leafem::addFeatures(
    m
    , data = x
    , radius = cex
    , radius_min_pixels = cex
    , radius_max_pixels = cex
    , stroke_width = lwd # * 100
    , stroke_opacity = alpha  * 255
    , fill_opacity = alpha.regions  * 255
    , stroke_colour = color
    , fill_colour = col.regions
    , tooltip = "label"
    , legend = legend
    , legend_options = list(title = layer.name)
    , layer_id = layer.name
    , ...
  )

  out <- new("mapview", object = list(sf::st_geometry(x)), map = m)

  return(out)

}

### sfc ###################################################################
leaflet_sfc <- function(x,
                        map,
                        pane,
                        canvas,
                        viewer.suppress,
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
                        highlight,
                        maxpoints,
                        attributes = NULL,
                        ...) {
  if (!is.null(names(x))) names(x) = NULL
  if (is_literally_false(highlight)) highlight = NULL
  if (is_literally_false(popup)) popup = NULL
  if (inherits(x, "XY")) x = sf::st_sfc(x)
  if (!native.crs) x <- checkAdjustProjection(x)
  if (is.na(sf::st_crs(x)$proj4string)) native.crs <- TRUE

  if (getGeometryType(x) %in% c("pl", "pt")) {
    if (is.function(col.regions)) col.regions <- standardColRegions(x)
  } else {
    if (is.function(color)) color <- standardColor(x)
  }

  if (is.null(map.types)) {
    if (getGeometryType(x) %in% c("pl", "pt")) {
      # if (is.function(col.regions)) col.regions <- standardColRegions(x)
      map.types <- basemaps(col.regions)
    } else {
      # if (is.function(color)) color <- standardColor(x)
      map.types <- basemaps(color)
    }
  }

  m <- initMap(
    map,
    map.types,
    sf::st_crs(x),
    native.crs,
    canvas = canvas,
    viewer.suppress = viewer.suppress
  )

  if (!canvas) {
    if (!is.null(pane)) {
      if (pane == "auto") {
        pane = paneName(x)
        zindex = zIndex(x)
        m = leaflet::addMapPane(m, pane, zindex)
      }
    }
  } else {
    pane = NULL
  }

  m <- leafem::addFeatures(m,
                           data = x,
                           pane = pane,
                           radius = cex,
                           weight = lwd,
                           opacity = alpha,
                           fillOpacity = alpha.regions,
                           color = color,
                           fillColor = col.regions,
                           popup = popup,
                           label = label,
                           group = layer.name,
                           highlightOptions = highlight,
                           native.crs = native.crs,
                           ...)

  if ("gl" %in% names(list(...)) &
      isTRUE(list(...)$gl) &
      inherits(sf::st_geometry(x), "sfc_POLYGON") &
      lwd > 0) {
    m = leafem::addFeatures(
      m
      , data = suppressWarnings(sf::st_cast(x, "LINESTRING"))
      , weight = lwd / 2
      , opacity = alpha
      , color = color
      , legend = FALSE
      , popup = NULL
      , group = layer.name
      , gl = TRUE
      , ...
    )
  }

  if (!is.null(map)) m = updateOverlayGroups(m, layer.name)
  sclbrpos = getCallEntryFromMap(m, "addScaleBar")
  if (length(sclbrpos) > 0 | native.crs) scalebar = FALSE else scalebar = TRUE

  funs <- list(if (scalebar) leaflet::addScaleBar,
               if (homebutton) leafem::addHomeButton,
               if (is.null(map)) mapViewLayersControl,
               leafem::addMouseCoordinates)
  funs <- funs[!sapply(funs, is.null)]

  args <- list(if (scalebar) list(position = "bottomleft"),
               if (homebutton) list(ext = createExtent(x),
                                    group = layer.name),
               if (is.null(map)) list(map.types = map.types,
                                      names = layer.name,
                                      native.crs = native.crs),
               list(style = "detailed",
                    epsg = sf::st_crs(x)$epsg,
                    proj4string = sf::st_crs(x)$proj4string,
                    native.crs = native.crs))
  args <- args[!sapply(args, is.null)]

  m <- decorateMap(map = m,
                   funs = funs,
                   args = args)

  try(
    if (attributes(popup)$popup == "leafpop") {
      m$dependencies <- c(m$dependencies, popupLayoutDependencies())
    }
    , silent = TRUE
  )

  if (is.function(legend)) m <- legend(m)
  m = removeDuplicatedMapDependencies(m)
  out <- new("mapview", object = list(x), map = m)

  return(out)

}


leafgl_sfc = function(x,
                      map,
                      zcol,
                      color,
                      col.regions,
                      at,
                      na.color,
                      cex,
                      lwd,
                      alpha,
                      alpha.regions,
                      na.alpha,
                      map.types,
                      verbose,
                      popup,
                      layer.name,
                      label,
                      legend,
                      legend.opacity,
                      homebutton,
                      native.crs,
                      highlight,
                      maxpoints,
                      viewer.suppress,
                      ...) {

  if (inherits(x, "XY")) x = sf::st_sfc(x)

  x = sf::st_sf(id = 1:length(x),
                jnk = 1L,
                geometry = sf::st_zm(x))

  if (!native.crs) x <- checkAdjustProjection(x)

  leafgl_sf(
    x = x
    , map = map
    , zcol = NULL
    , color = color
    , col.regions = col.regions
    , at = at
    , na.color = na.color
    , cex = cex
    , lwd = lwd
    , alpha = alpha
    , alpha.regions = alpha.regions
    , na.alpha = na.alpha
    , map.types = map.types
    , verbose = verbose
    , popup = popup
    , layer.name = layer.name
    , label = label
    , legend = legend
    , legend.opacity = legend.opacity
    , homebutton = homebutton
    , native.crs = native.crs
    , hightlight = highlight
    , maxpoints = maxpoints
    , viewer.suppress = viewer.suppress
    , ...
  )

}



mapdeck_sfc = function(x,
                       map,
                       zcol,
                       color,
                       col.regions,
                       at,
                       na.color,
                       cex,
                       lwd,
                       alpha,
                       alpha.regions,
                       na.alpha,
                       map.types,
                       verbose,
                       popup,
                       layer.name,
                       label,
                       legend,
                       legend.opacity,
                       homebutton,
                       native.crs,
                       highlight,
                       maxpoints,
                       viewer.suppress,
                       ...) {

  if (inherits(x, "XY")) x = sf::st_sfc(x)

  x = sf::st_sf(id = as.character(1:length(x)),
                jnk = 1L,
                geometry = x)

  if (!native.crs) x <- checkAdjustProjection(x)

  mapdeck_sf(
    x = x
    , map = map
    , zcol = NULL
    , color = color
    , col.regions = col.regions
    , at = at
    , na.color = na.color
    , cex = cex
    , lwd = lwd
    , alpha = alpha
    , alpha.regions = alpha.regions
    , na.alpha = na.alpha
    , map.types = map.types
    , verbose = verbose
    , popup = popup
    , layer.name = layer.name
    , label = "id"
    , legend = FALSE
    , legend.opacity = legend.opacity
    , homebutton = homebutton
    , native.crs = native.crs
    , hightlight = highlight
    , maxpoints = maxpoints
    , viewer.suppress = viewer.suppress
    , ...
  )

}


### MISC ==================================================================
sf2DataFrame <- function(x, drop_sf_column = FALSE) {
  stopifnot(inherits(x, "sf") | inherits(x, "sfc"))
  if (inherits(x, "sf")) {
    if (drop_sf_column) {
      return(as.data.frame(x)[setdiff(names(x), attr(x, "sf_column"))])
      # geompos <- which(names(x) == attr(x, "sf_column"))
      # return(data.frame(x)[, -geompos, drop = FALSE])
    } else return(as.data.frame(x))
  } else {
    d <- data.frame("a" = seq(length(x)))
    names(d) <- "Feature ID"
    return(d)
  }
}


# nNodes = function(x) {
#   sum(sapply(x, function(y) {
#     if (is.list(y)) nNodes(y) else nrow(y)
#   }))
# }
# nNodes = function(x) {
#   sapply(
#     sapply(x, function(y) {
#       if (is.list(y)) nNodes(y) else nrow(y)
#     }),
#     sum
#   )
# }
nNodes = function(x) length(unlist(sf::st_geometry(x), use.names = FALSE)) / 2

# nPoints = function(x) {
#   if (getGeometryType(x) == "pt") {
#     length(sf::st_geometry(x))
#   } else {
#     nNodes(sf::st_geometry(x))
#   }
# }

nVerts = function(x) {
  out = if (is.list(x)) sapply(sapply(x, nVerts), sum) else {
    if (is.matrix(x))
      nrow(x)
    else {
      if (sf::st_is_empty(x)) 0 else 1
    }
  }
  unname(out)
}

#' count the number of points/vertices/nodes of sf objects
#' @param x an sf/sfc object
#' @param by_feature count total number of vertices (FALSE) of for each feature (TRUE).
#'
#' @note currently only works for *POINTS, *LINES and *POLYGONS (not GEOMETRYCOLLECTION).
#'
#' @export
#'
#' @examples
#' npts(franconia)
#' npts(franconia, by_feature = TRUE)
#' npts(sf::st_geometry(franconia[1, ])) # first polygon
#'
#' npts(breweries) # is the same as
#' nrow(breweries)
#'
npts = function(x, by_feature = FALSE) {
  if (by_feature) nVerts(sf::st_geometry(x)) else sum(nVerts(sf::st_geometry(x)))
}




# nfeats = function(x) {
#   if (inherits(x, "sf")) nrow(x) else length(x)
# }

nrings = function(pol) {
  if (inherits(pol, "MULTIPOLYGON"))
    return(sum(lengths(pol)))
  pol = sf::st_geometry(pol)
  if (inherits(pol, "sfc_MULTIPOLYGON"))
    return(do.call(sum, lapply(pol, lengths)))
  if (inherits(pol, "sfc_POLYGON"))
    return(sum(lengths(pol)))
}

# polygonComplexity = function(pol) {
#   nrings(pol) + npts(pol) + nfeats(pol)
# }

# lineComplexity = function(ln) {
#   npts(ln) + nfeats(ln)
# }

featureComplexity = function(x) {
  if (inherits(x, "sf")) {
    dm = dim(x)
    switch(
      getGeometryType(x),
      "pt" = nNodes(x) / 1e6 * dm[1] * dm[2],
      "ln" = nNodes(x) / 1e6 * dm[1] * dm[2],
      "pl" = nNodes(x) / 1e6 * nrings(x) * dm[2],
      "gc" = nNodes(x) / 1e6 * dm[1] * dm[2]
    )
  } else {
    switch(
      getGeometryType(x),
      "pt" = nNodes(x) / 1e6 * length(x),
      "ln" = nNodes(x) / 1e6 * length(x),
      "pl" = nNodes(x) / 1e6 * nrings(x),
      "gc" = nNodes(x) / 1e6 * length(x)
    )
  }
}
