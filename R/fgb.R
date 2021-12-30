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
                 hide,
                 ...) {

  if (is_literally_false(popup)) popup = NULL
  # if (!(is.null(attr(popup, "popup"))) &&
  #     attr(popup, "popup") == "leafpop") {
  #   popup = TRUE
  # }

  if (!native.crs) x <- checkAdjustProjection(x)
  if (is.na(sf::st_crs(x)$proj4string)) native.crs <- TRUE

  ### color needs work when we have chromajs working in leafem::addFgb
  color <- vectorColors(x = x,
                        zcol = zcol,
                        colors = color,
                        at = at,
                        na.color = na.color)
  col.regions <- vectorColRegions(x = x,
                                  zcol = zcol,
                                  col.regions = col.regions,
                                  at = at,
                                  na.color = na.color)

  if (is.null(map.types) |
      identical(mapviewGetOption("basemaps"), map.types)) {
    if (getGeometryType(x) %in% c("pl", "pt")) {
      map.types <- as.vector(stats::na.omit(basemaps(col.regions)))
    } else {
      map.types <- as.vector(stats::na.omit(basemaps(color)))
    }
  }

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

  if (getGeometryType(x) == "ln")
    col.regions = NULL

  ## color
  if (length(color) > 1 &&
      length(color) == nrow(x) &&
      !(length(unique(color)) == 1)) {
    x$color = color
    color = NULL
  } else {
    color = unique(color)
  }

  scaleList = list()

  ## size
  if (inherits(cex, "character") &&
      cex %in% colnames(x) &&
      is.numeric(x[[cex]])) {
    scaleList = utils::modifyList(
      scaleList
      , list(
        radius = list(
          to = c(3, 15)
          , from = range(x[[cex]], na.rm = TRUE)
        )
      )
    )
  }

  ## width
  if (inherits(lwd, "character") &&
      lwd %in% colnames(x) &&
      is.numeric(x[[lwd]])) {
    scaleList = utils::modifyList(
      scaleList
      , list(
        weight = list(
          to = c(1, 10)
          , from = range(x[[lwd]], na.rm = TRUE)
        )
      )
    )
  }

  ## opacity
  if (inherits(alpha, "character") &&
      alpha %in% colnames(x) &&
      is.numeric(x[[alpha]])) {
    scaleList = utils::modifyList(
      scaleList
      , list(
        opacity = list(
          to = c(0.1, 1)
          , from = range(x[[alpha]], na.rm = TRUE)
        )
      )
    )
  }

  ## fillOpacity
  if (inherits(alpha.regions, "character") &&
      alpha.regions %in% colnames(x) &&
      is.numeric(x[[alpha.regions]])) {
    scaleList = utils::modifyList(
      scaleList
      , list(
        fillOpacity = list(
          to = c(0.1, 1)
          , from = range(x[[alpha.regions]], na.rm = TRUE)
        )
      )
    )
  }

  ## label
  if (is.null(label)) {
    if (is.null(zcol)) {
      label = "mvFeatureId"
    } else {
      label = zcol
    }
  }

  # add mapview feature id to x (needed in several JS steps)
  x$mvFeatureId = 1:nrow(x)

  fl = tempfile(fileext = ".fgb")
  sf::st_write(
    obj = x
    , dsn = fl
    , driver = "FlatGeobuf"
    , layer_options = c("SPATIAL_INDEX=NO")
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

  if (getGeometryType(x) == "ln") col.regions = NULL

  m = leafem::addFgb(
    map = m
    , file = fl
    , radius = cex
    , weight = lwd
    , opacity = alpha
    , fillOpacity = alpha.regions
    , color = color
    , fillColor = col.regions
    , popup = popup
    , label = label
    , group = layer.name
    , fill = ifelse(getGeometryType(x) == "ln", FALSE, TRUE)
    , className = "mapview-popup"
    , scale = scaleList
    , options = leaflet::pathOptions(
      pane = pane
    )
    , ...
  )

  if (!is.null(map)) m = updateOverlayGroups(m, layer.name)
  sclbrpos = getCallEntryFromMap(m, "addScaleBar")
  if (length(sclbrpos) > 0 | native.crs) scalebar = FALSE else scalebar = TRUE

  funs <- list(
    if (scalebar) leaflet::addScaleBar
    , if (homebutton) leafem::addHomeButton
    , if (is.null(map)) mapViewLayersControl
    , leafem::addMouseCoordinates
    , leafem::addCopyExtent
  )
  funs <- funs[!sapply(funs, is.null)]

  args <- list(
    if (scalebar) list(position = "bottomleft")
    , if (homebutton) list(
      ext = createExtent(x)
      , group = layer.name
      , position = mapviewGetOption("homebutton.pos")
    )
    , if (is.null(map)) list(
      map.types = map.types
      , names = layer.name
      , native.crs = native.crs
    )
    , list(
      style = "detailed"
      , epsg = sf::st_crs(x)$epsg
      , proj4string = sf::st_crs(x)$proj4string
      , native.crs = native.crs
    )
    , list(
      event.code = "KeyE"
    )
  )
  args <- args[!sapply(args, is.null)]


  m <- decorateMap(map = m,
                   funs = funs,
                   args = args)

  if (is.function(legend)) m <- legend(m)
  m = removeDuplicatedMapDependencies(m)

  if (hide) {
    m = leaflet::hideGroup(m, layer.name)
  }

  bb = unname(sf::st_bbox(x))

  # if bbox too small, restrict zoom to 18
  if (identical(bb[1], bb[3])) {
    m = leaflet::setView(
      m
      , lng = mean(bb[1], bb[3], na.rm = TRUE)
      , lat = mean(bb[2], bb[4], na.rm = TRUE)
      , zoom = 18
    )
  } else {
    m = leaflet::fitBounds(
      m
      , bb[1]
      , bb[2]
      , bb[3]
      , bb[4]
    )
  }

  m$dependencies = c(
    m$dependencies
    , mapviewCSSDependencies()
  )

  out <- new("mapview", object = list(sf::st_geometry(x)), map = m)

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
