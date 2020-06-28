#### functions for leaflet based rendering by spatial class


#### RASTER ###############################################################
###########################################################################
### leaflet w RasterLayer =================================================

leafletRL = function(x,
                     map,
                     maxpixels,
                     col.regions,
                     at,
                     na.color,
                     use.layer.names,
                     map.types,
                     alpha.regions,
                     legend,
                     legend.opacity,
                     trim,
                     verbose,
                     layer.name,
                     homebutton,
                     native.crs,
                     method,
                     label,
                     query.type,
                     query.digits,
                     query.position,
                     query.prefix,
                     viewer.suppress,
                     ...) {

  if (inherits(map, "mapview")) map = mapview2leaflet(map)
  if (is.null(layer.name)) layer.name = makeLayerName(x, zcol = NULL)

  pkgs = c("leaflet", "raster", "magrittr")
  tst = sapply(pkgs, "requireNamespace",
               quietly = TRUE, USE.NAMES = FALSE)

  if (native.crs) {

    if (!requireNamespace("plainview", quietly = TRUE)) {
      stop(
        paste("\nviewing rasters with native CRS requires package 'plainview'.\n",
              "To install use install.packages('plainview')"),
        call. = FALSE
      )
    }

    plainview::plainView(x,
                         maxpixels = mapviewGetOption("plainview.maxpixels"),
                         col.regions = col.regions,
                         at = at,
                         na.color = na.color,
                         legend = legend,
                         verbose = verbose,
                         layer.name = layer.name,
                         gdal = TRUE,
                         ...)
  } else {

    is.fact = raster::is.factor(x)

    if (use.layer.names) {
      grp = names(x)
      layer.name = names(x)
    } else {
      grp = layer.name
    }
    x = rasterCheckSize(x, maxpixels = maxpixels)
    x = rasterCheckAdjustProjection(x, method)
    ext = raster::extent(raster::projectExtent(x, crs = llcrs))

    if (!is.na(raster::projection(x)) & trim) x = trim(x)

    m = initMap(map, map.types, sp::proj4string(x), viewer.suppress = viewer.suppress)

    if (!is.function(col.regions)) {
      col.regions = grDevices::colorRampPalette(col.regions)
    }

    if (is.fact) {
      vals = as.factor(x@data@attributes[[1]]$ID)
      pal = leaflet::colorFactor(palette = col.regions(length(vals)),
                                 domain = vals,
                                 na.color = na.color)
    } else {
      pal = rasterColors(col.regions,
                         at = at,
                         na.color = na.color)

    }

    m = leafem::garnishMap(
      map = m
      , leaflet::addRasterImage
      , x = x
      , colors = pal
      , project = FALSE
      , opacity = alpha.regions
      , group = grp
      , layerId = grp
      , ...
    )

    m = removeLayersControl(m)
    m = mapViewLayersControl(map = m,
                             map.types = map.types,
                             names = grp)

    if (label)
      m = leafem::addImageQuery(m, x, group = grp, layerId = grp,
                                type = query.type, digits = query.digits,
                                position = query.position, prefix = query.prefix)
    if (legend) {
      if (!is.fact) {
        leg_vals = x[]
        leg_clrs = col.regions
      } else {
        if (ncol(x@data@attributes[[1]]) >= 2) {
          args = list(...)
          if ("att" %in% names(args)) att = args$att else att = 2
          leg_vals = factor(
            x@data@attributes[[1]][[att]]
            , levels = x@data@attributes[[1]][[att]]
          )
        } else {
          leg_vals = as.factor(x[])
        }
        leg_clrs = col.regions(length(levels(vals)))
      }
      legend = mapviewLegend(values = leg_vals,
                             colors = leg_clrs,
                             at = at,
                             na.color = col2Hex(na.color),
                             layer.name = layer.name)

      m = legend(m)

    }

    sclbrpos = getCallEntryFromMap(m, "addScaleBar")
    if (length(sclbrpos) > 0 | native.crs) scalebar = FALSE else scalebar = TRUE
    if (scalebar) m = leaflet::addScaleBar(m, position = "bottomleft")
    m = leafem::addMouseCoordinates(m)
    if (homebutton) m = leafem::addHomeButton(m, ext, group = layer.name)

    out = new('mapview', object = list(x), map = m)

    return(out)

  }

}



### leaflet w RasterStackBrick ============================================

leafletRSB = function(x,
                      map,
                      maxpixels,
                      col.regions,
                      at,
                      na.color,
                      use.layer.names,
                      map.types,
                      legend,
                      legend.opacity,
                      trim,
                      verbose,
                      layer.name,
                      homebutton,
                      method,
                      label,
                      query.type,
                      query.digits,
                      query.position,
                      query.prefix,
                      viewer.suppress,
                      ...) {

  pkgs = c("leaflet", "raster", "magrittr")
  tst = sapply(pkgs, "requireNamespace",
               quietly = TRUE, USE.NAMES = FALSE)

  if (inherits(map, "mapview")) map = mapview2leaflet(map)
  m = initMap(map, map.types, sp::proj4string(x), viewer.suppress = viewer.suppress)

  if (nlayers(x) == 1) {
    x = raster(x, layer = 1)
    m = mapView(x,
                map = m,
                maxpixels = maxpixels,
                map.types = map.types,
                use.layer.names = use.layer.names,
                at = at,
                col.regions = col.regions,
                na.color = na.color,
                legend = legend,
                layer.name = layer.name,
                homebutton = homebutton,
                method = method,
                label = label,
                query.type = query.type,
                query.digits = query.digits,
                query.position = query.position,
                query.prefix = query.prefix,
                ...)
    out = new('mapview', object = list(x), map = m@map)
  } else {
    m = mapView(x[[1]],
                map = m,
                maxpixels = maxpixels,
                map.types = map.types,
                use.layer.names = use.layer.names,
                at = at,
                col.regions = col.regions,
                na.color = na.color,
                legend = legend,
                homebutton = homebutton,
                method = method,
                label = label,
                query.type = query.type,
                query.digits = query.digits,
                query.position = query.position,
                query.prefix = query.prefix,
                ...)
    for (i in 2:nlayers(x)) {
      m = mapView(x[[i]],
                  map = m@map,
                  maxpixels = maxpixels,
                  map.types = map.types,
                  use.layer.names = use.layer.names,
                  at = at,
                  col.regions = col.regions,
                  na.color = na.color,
                  legend = legend,
                  homebutton = FALSE,
                  method = method,
                  label = label,
                  query.type = query.type,
                  query.digits = query.digits,
                  query.position = query.position,
                  query.prefix = query.prefix,
                  ...)
    }

    if (length(getLayerNamesFromMap(m@map)) > 1) {
      m = leaflet::hideGroup(map = m@map,
                             group = layers2bHidden(m@map))
    }
    out = new('mapview', object = list(x), map = m)
  }

  return(out)

}



### leaflet w SpatialPixelsDataFrame ======================================

leafletPixelsDF = function(x,
                           map,
                           zcol,
                           maxpixels,
                           col.regions,
                           at,
                           na.color,
                           use.layer.names,
                           map.types,
                           alpha.regions,
                           legend,
                           legend.opacity,
                           trim,
                           verbose,
                           layer.name,
                           homebutton,
                           native.crs,
                           method,
                           label,
                           query.type,
                           query.digits,
                           query.position,
                           query.prefix,
                           viewer.suppress,
                           ...) {

  pkgs = c("leaflet", "sp", "magrittr")
  tst = sapply(pkgs, "requireNamespace",
               quietly = TRUE, USE.NAMES = FALSE)

  if (inherits(map, "mapview")) map = mapview2leaflet(map)
  if(!is.null(zcol)) x = x[, zcol]

  stck = do.call("stack", lapply(seq(ncol(x)), function(i) {
    r = raster::raster(x[, i])
    if (is.factor(x[, i])) r = raster::as.factor(r)
    return(r)
  }))
  if(is.null(layer.name)) names(stck) = zcol else names(stck) = layer.name

  m = mapView(stck,
              map = map,
              maxpixels = maxpixels,
              col.regions = col.regions,
              at = at,
              na.color = na.color,
              use.layer.names = TRUE,
              map.types = map.types,
              alpha.regions = alpha.regions,
              legend = legend,
              legend.opacity = legend.opacity,
              trim = trim,
              verbose = verbose,
              layer.name = layer.name,
              homebutton = homebutton,
              native.crs = native.crs,
              method = method,
              label = label,
              query.type = query.type,
              query.digits = query.digits,
              query.position = query.position,
              query.prefix = query.prefix,
              viewer.suppress = viewer.suppress,
              ...)

  out = new('mapview', object = list(x), map = m@map)

  return(out)

}





### leaflet w Satellite ===================================================

leafletSatellite = function(x, ...) {

  pkgs = c("leaflet", "satellite", "magrittr")
  tst = sapply(pkgs, "requireNamespace",
               quietly = TRUE, USE.NAMES = FALSE)

  m = mapView(stack(x), ...)

  out = new('mapview', object = list(x), map = m@map)

  return(out)

}




# Convert RasterLayers to png or RasterStacks/Bricks to RGB png

## raster layer -----------------------------------------------------------
raster2PNG <- function(x,
                       col.regions,
                       at,
                       na.color,
                       maxpixels) {

  x <- rasterCheckSize(x, maxpixels = maxpixels)

  mat <- t(raster::as.matrix(x))

  if (missing(at)) at <- lattice::do.breaks(range(mat, na.rm = TRUE), 256)

  cols <- lattice::level.colors(mat,
                                at = at,
                                col.regions = col.regions)
  cols[is.na(cols)] = na.color
  cols = col2Hex(cols, alpha = TRUE)
  #cols <- clrs(t(mat))
  png_dat <- as.raw(grDevices::col2rgb(cols, alpha = TRUE))
  dim(png_dat) <- c(4, ncol(x), nrow(x))

  return(png_dat)
}


## raster stack/brick -----------------------------------------------------

rgbStack2PNG <- function(x, r, g, b,
                         na.color,
                         quantiles = c(0.02, 0.98),
                         maxpixels,
                         ...) {

  x <- rasterCheckSize(x, maxpixels = maxpixels)

  x3 <- raster::subset(x, c(r, g, b))

  mat <- cbind(x[[r]][],
               x[[g]][],
               x[[b]][])

  for(i in seq(ncol(mat))){
    z <- mat[, i]
    lwr <- stats::quantile(z, quantiles[1], na.rm = TRUE)
    upr <- stats::quantile(z, quantiles[2], na.rm = TRUE)
    z <- (z - lwr) / (upr - lwr)
    z[z < 0] <- 0
    z[z > 1] <- 1
    mat[, i] <- z
  }

  na_indx = apply(mat, 1, base::anyNA) # na_indx <- rowNA(mat)
  cols <- rep(na.color, nrow(mat)) #mat[, 1] #
  #cols[na_indx] <- na.color
  cols[!na_indx] <- grDevices::rgb(mat[!na_indx, ], alpha = 1)
  png_dat <- as.raw(grDevices::col2rgb(cols, alpha = TRUE))
  dim(png_dat) <- c(4, ncol(x), nrow(x))

  return(png_dat)
}
