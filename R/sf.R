# large_warn = paste("\nthe supplied feature layer seems quite large.\n",
#                    "would you like to view in the browser instead of RStudio viewer? (recommended)\n")

### sf ####################################################################
leaflet_sf <- function(x,
                       map,
                       pane,
                       canvas,
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

  if (inherits(st_geometry(x), "sfc_MULTIPOINT"))
    x = suppressWarnings(st_cast(x, "POINT"))

  if (is.null(layer.name)) layer.name = makeLayerName(x, zcol)
  cex <- circleRadius(x, cex)
  if (is.null(zcol) & ncol(sf2DataFrame(x, drop_sf_column = TRUE)) == 1) {
    zcol = colnames(sf2DataFrame(x, drop_sf_column = TRUE))[1]
    label = makeLabels(x, zcol)
  }
  if (!is.null(zcol)) {
    if (length(unique(x[[zcol]])) == 1) {
      color = ifelse(is.function(color), standardColor(x), color)
      col.regions = ifelse(is.function(col.regions), standardColRegions(x), col.regions)
    }
  }
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

  #   # layer.name <- paste(layer.name, zcol)
  #   if (length(unique(x[[zcol]])) <= 1) {
  #     warning(
  #       sprintf(
  #         "column %s has only one unique value/level, ignoring color and legend",
  #         zcol
  #       )
  #     )
  #     # zcol <- NULL
  #   }
  # }

  # if (!native.crs) x <- checkAdjustProjection(x)

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
              ...)

}


### sfc ###################################################################
leaflet_sfc <- function(x,
                        map,
                        pane,
                        canvas,
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
  if (!is.null(names(x))) {
    names(x) = NULL
  }
  if (!highlight) highlight = NULL
  if (inherits(x, "XY")) x = sf::st_sfc(x)

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

  m <- initMap(map, map.types, sf::st_crs(x), native.crs, canvas = canvas)

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

  m <- addFeatures(m,
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
                   ...)

  if (!is.null(map)) m = updateOverlayGroups(m, layer.name)
  sclbrpos = getCallEntryFromMap(m, "addScaleBar")
  if (length(sclbrpos) > 0 | native.crs) scalebar = FALSE else scalebar = TRUE

  funs <- list(if (scalebar) leaflet::addScaleBar,
               if (homebutton) addHomeButton,
               if (is.null(map)) mapViewLayersControl,
               addMouseCoordinates)
  funs <- funs[!sapply(funs, is.null)]

  args <- list(if (scalebar) list(position = "bottomleft"),
               if (homebutton) list(ext = createExtent(x),
                                    layer.name = layer.name),
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
    if (attributes(popup)$popup == "mapview") {
      m$dependencies <- c(m$dependencies, popupLayoutDependencies())
    }
    , silent = TRUE
  )

  if (is.function(legend)) m <- legend(m)
  m = removeDuplicatedMapDependencies(m)
  out <- new("mapview", object = list(x), map = m)

  return(out)

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
nNodes = function(x) {
  sapply(
    sapply(x, function(y) {
      if (is.list(y)) nNodes(y) else nrow(y)
    }),
    sum
  )
}


nPoints = function(x) {
  if (getGeometryType(x) == "pt") {
    length(sf::st_geometry(x))
  } else {
    nNodes(sf::st_geometry(x))
  }
}

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




nfeats = function(x) {
  if (inherits(x, "sf")) nrow(x) else length(x)
}

nrings = function(pol) {
  do.call(sum, lapply(sf::st_geometry(pol), lengths))
}

polygonComplexity = function(pol) {
  nrings(pol) + npts(pol) + nfeats(pol)
}

lineComplexity = function(ln) {
  npts(ln) + nfeats(ln)
}

featureComplexity = function(x) {
  switch(getGeometryType(x),
         "pt" = nPoints(x),
         "ln" = lineComplexity(x),
         "pl" = polygonComplexity(x),
         "gc" = polygonComplexity(x))
}
