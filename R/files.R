### tiles ----
leaflet_tiles = function(x,
                         map,
                         tms = TRUE,
                         map.types,
                         verbose,
                         layer.name,
                         homebutton,
                         native.crs,
                         viewer.suppress,
                         ...) {

  if (inherits(map, "mapview")) map = mapview2leaflet(map)
  if (is.null(layer.name)) layer.name = makeLayerName(basename(x), zcol = NULL)

  if (native.crs) {

    stop(
      paste("\nviewing of raster tiles currently only supported for longlat data"),
      call. = FALSE
    )
  }

  # if (is.null(map.types)) map.types = basemaps("#000000")

  m = initMap(map,
              map.types,
              "+proj=longlat +datum=WGS84 +no_defs",
              viewer.suppress = viewer.suppress)

  m = leafem::addTileFolder(map = m,
                            folder = x,
                            group = layer.name,
                            tms = tms,
                            ...)


  fldrs = list.dirs(x, recursive = FALSE)
  bsn = basename(fldrs)

  zooms = as.numeric(bsn)
  mnzm = min(zooms)

  fldr_mnzm = fldrs[basename(fldrs) == as.character(mnzm)]
  fldrs_mnzm = list.dirs(fldr_mnzm, recursive = TRUE)[-1]

  fldrs_mnzm_mn = fldrs_mnzm[which.min(as.numeric(basename(fldrs_mnzm)))]
  fldrs_mnzm_mx = fldrs_mnzm[which.max(as.numeric(basename(fldrs_mnzm)))]

  x_mn = min(as.numeric(basename(fldrs_mnzm_mn)))
  x_mx = max(as.numeric(basename(fldrs_mnzm_mx)))

  tiles_mn = list.files(fldrs_mnzm_mn)
  y_mn = min(as.numeric(tools::file_path_sans_ext(tiles_mn)))
  y_mn = (2^mnzm) - y_mn - 1

  tiles_mx = list.files(fldrs_mnzm_mx)
  y_mx = max(as.numeric(tools::file_path_sans_ext(tiles_mx)))
  y_mx = (2^mnzm) - y_mx - 1

  ll_mn = tilenum_to_lonlat(x_mn, y_mn + 1, mnzm)
  ll_mx = tilenum_to_lonlat(x_mx, y_mx, mnzm)
  #
  # m = leaflet::addTiles(
  #   map = m,
  #   urlTemplate = paste0(
  #     "lib/",
  #     basename(x),
  #     "-0.0.1/{z}/{x}/{y}.png"
  #   ),
  #   group = layer.name,
  #   options = tileOptions(
  #     minZoom = min(zooms),
  #     maxZoom = 18,
  #     tms = TRUE,
  #     maxNativeZoom = max(zooms)
  #   )
  # )
  #
  m = leaflet::setView(
    map = m,
    lng = diff(c(ll_mn$lon, ll_mx$lon)) / 2 + ll_mn$lon,
    lat = diff(c(ll_mn$lat, ll_mx$lat)) / 2 + ll_mn$lat,
    zoom = mnzm
  )

  m = mapViewLayersControl(map = m,
                           map.types = map.types,
                           names = layer.name)

  sclbrpos = getCallEntryFromMap(m, "addScaleBar")
  if (length(sclbrpos) > 0 | native.crs) scalebar = FALSE else scalebar = TRUE
  if (scalebar) m = leaflet::addScaleBar(m, position = "bottomleft")
  m = leafem::addMouseCoordinates(m)

  if (homebutton) {
    m = leafem::addHomeButton(
      m,
      ext = raster::extent(ll_mn$lon, ll_mx$lon, ll_mn$lat, ll_mx$lat),
      group = layer.name
    )
  }
  #
  # m$dependencies =  c(m$dependencies, tiledDataDependency(x))

  if (!is.null(map)) map = updateOverlayGroups(map, layer.name)

  out = new('mapview', object = list(x), map = m)

  return(out)

}


### file ----
leaflet_file = function(x,
                        map,
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
                        layer.name,
                        homebutton,
                        native.crs,
                        canvas,
                        viewer.suppress,
                        ...) {

  # if (is.null(map.types)) {
  #   map.types = basemaps(standardColor())
  # }

  m <- initMap(
    map,
    map.types = map.types,
    sf::st_crs(4326),
    native.crs = native.crs,
    canvas = canvas,
    viewer.suppress = viewer.suppress
  )

  m = leafem::addLocalFile(
    map = m,
    file = x,
    group = layer.name,
    radius = cex,
    stroke = TRUE,
    color = color,
    weight = lwd,
    opacity = alpha,
    fill = TRUE,
    fillColor = col.regions,
    fillOpacity = alpha.regions,
    dashArray = NULL,
    options = NULL
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
               if (homebutton) list(group = layer.name),
               if (is.null(map)) list(map.types = map.types,
                                      names = layer.name,
                                      native.crs = native.crs),
               list(epsg = 4326,
                    proj4string = "longlat",
                    native.crs = FALSE))
  args <- args[!sapply(args, is.null)]

  m <- decorateMap(map = m,
                   funs = funs,
                   args = args)

  m = removeDuplicatedMapDependencies(m)
  out = new('mapview', object = list(x), map = m)

  return(out)

}


### utils ----
tiledDataDependency <- function(tiles_dir) {
  list(
    htmltools::htmlDependency(
      name = basename(tiles_dir),
      version = "0.0.1",
      src = c(file = tiles_dir)
    )
  )
}

degrees <- function(angle_rad) (angle_rad * 180) / pi

tilenum_to_lonlat <- function(x, y, zoom){
  n_tiles <- 2^zoom

  lon_rad <- (((x / n_tiles) * 2) - 1) * pi

  merc_lat <- (1 - ((y / n_tiles) * 2)) * pi
  lat_rad <- atan(sinh(merc_lat))

  list(lon = degrees(lon_rad),
       lat = degrees(lat_rad))
}
