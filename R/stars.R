addStars <- function(
  map,
  x,
  bounds,
  colors = "Spectral",
  opacity = 1,
  attribution = NULL,
  layerId = NULL,
  group = NULL,
  project = FALSE,
  maxBytes = 4*1024*1024
) {
  stopifnot(inherits(x, "stars"))

  # if (project) {
  #   projected <- projectRasterForLeaflet(x)
  # } else {
  projected <- x
  # }
  # bounds <- raster::extent(raster::projectExtent(raster::projectExtent(x, crs = sp::CRS(epsg3857)), crs = sp::CRS(epsg4326)))

  if (!is.function(colors)) {
    colors <- colorNumeric(colors, domain = NULL, na.color = "#00000000", alpha = TRUE)
  }

  tileData <- as.numeric(projected[[1]]) %>%
    colors() %>% col2rgb(alpha = TRUE) %>% as.raw()
  dim(tileData) <- c(4, nrow(projected), ncol(projected))
  pngData <- png::writePNG(tileData)
  if (length(pngData) > maxBytes) {
    stop("Raster image too large; ", length(pngData), " bytes is greater than maximum ", maxBytes, " bytes")
  }
  encoded <- base64enc::base64encode(pngData)
  uri <- paste0("data:image/png;base64,", encoded)

  latlng <- list(
    list(raster::ymax(bounds), raster::xmin(bounds)),
    list(raster::ymin(bounds), raster::xmax(bounds))
  )

  invokeMethod(map, getMapData(map), "addRasterImage", uri, latlng, opacity, attribution, layerId, group) %>%
    expandLimits(c(raster::ymin(bounds), raster::ymax(bounds)), c(raster::xmin(bounds), raster::xmax(bounds)))
}
