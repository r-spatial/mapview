sfFgb = function(x,
                 map,
                 pane,
                 zcol,
                 color,
                 col.regions,
                 at,
                 na.color,
                 cex,
                 lwd,
                 alpha,
                 alpha.regions,
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
                 attributes,
                 canvas,
                 viewer.suppress,
                 ...) {

  popup_columns = colnames(sf::st_drop_geometry(x))

  if (!native.crs) x <- checkAdjustProjection(x)
  if (is.na(sf::st_crs(x)$proj4string)) native.crs <- TRUE

  ## fillColor
  if (length(col.regions) > 1 &&
      length(col.regions) == nrow(x) &&
      !(length(unique(col.regions)) == 1) &&
      getGeometryType(x) != "ln") {
    x$fillColor = col.regions
    col.regions = NULL
  } else {
    col.regions = unique(col.regions)
  }

  ## color
  if (length(color) > 1 &&
      length(color) == nrow(x) &&
      !(length(unique(color)) == 1)) {
    x$color = color
    color = NULL
  } else {
    color = unique(color)
  }

  ## size
  if (length(cex) > 1 &&
      length(cex) == nrow(x) &&
      !(length(unique(cex)) == 1)) {
    x$radius = cex
    cex = NULL
  } else {
    cex = unique(cex)
  }

  ## width
  if (length(lwd) > 1 &&
      length(lwd) == nrow(x) &&
      !(length(unique(lwd)) == 1)) {
    x$weight = lwd
    lwd = NULL
  } else {
    lwd = unique(lwd)
  }

  ## opacity

  if (length(alpha) > 1 &&
      length(alpha) == nrow(x) &&
      !(length(unique(alpha)) == 1)) {
    x$opacity = alpha
    alpha = NULL
  } else {
    alpha = unique(alpha)
  }

  ## fillOpacity
  if (length(alpha.regions) > 1 &&
      length(alpha.regions) == nrow(x) &&
    !(length(unique(alpha.regions)) == 1)) {
    x$fillOpacity = alpha.regions
    alpha.regions = NULL
  } else {
    alpha.regions = unique(alpha.regions)
  }

  fl = tempfile(fileext = ".fgb")
  sf::st_write(
    obj = x
    , dsn = fl
    , driver = "FlatGeobuf"
    , append = FALSE
    , quiet = TRUE
  )

  m <- initMap(
    map,
    map.types,
    sf::st_crs(x),
    native.crs,
    canvas = canvas,
    viewer.suppress = viewer.suppress
  )

  m = leafem::addFgb(
    map = m
    , file = fl
    , radius = cex
    , weight = lwd
    , opacity = alpha
    , fillOpacity = alpha.regions
    , color = color
    , fillColor = col.regions
    , popup = popup_columns
    , label = NULL
    , group = layer.name
    , fill = ifelse(getGeometryType(x) == "ln", FALSE, TRUE)
    , className = "mapview-popup"
  )

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

  if (is.function(legend)) m <- legend(m)
  m = removeDuplicatedMapDependencies(m)

  bb = unname(sf::st_bbox(x))

  m = leaflet::fitBounds(
    m
    , bb[1]
    , bb[2]
    , bb[3]
    , bb[4]
  )

  m$dependencies = c(
    m$dependencies
    , mapviewCSSDependencies()
  )

  out <- new("mapview", object = list(x), map = m)

  return(out)


}


mapviewCSSDependencies = function() {
  list(
    htmltools::htmlDependency(
      "mapviewCSS"
      , '0.0.1'
      , system.file("htmlwidgets/lib/css", package = "mapview")
      , stylesheet = c('mapview-popup.css', "mapview.css")
    )
  )
}
