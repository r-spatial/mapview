mapgl_sf <- function(
  x,
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
  ...
) {
  # Initialize map centered on data
  # TODO: Add switch for mapgl::mapboxgl depending on
  # mapviewOptions("mapgl.platform") or similar
  m <- mapgl::maplibre(bounds = x)

  # Add source
  m <- mapgl::add_source(m, "data", x)

  # Determine geometry type
  geom_type <- getGeometryType(x)

  # Handle color inheritance from mapview settings
  if (geom_type %in% c("pl", "pt")) {
    if (is.function(col.regions)) col.regions <- standardColRegions(x)
  } else {
    if (is.function(color)) color <- standardColor(x)
  }

  if (!is.null(zcol)) {
    if (!is.null(color)) {
      color <- ifelse(geom_type %in% c("pl", "pt"), standardColor(x), zcol)
    }
    col.regions <- ifelse(geom_type %in% c("pl", "pt"), zcol, standardColor(x))
  } else {
    if (!is.null(color)) {
      color <- ifelse(is.function(color), standardColor(x), color)
    }
    col.regions <- ifelse(
      is.function(col.regions),
      standardColRegions(x),
      col.regions
    )
  }

  # Handle alpha inheritance
  if (!is.null(zcol) & !is.null(na.alpha)) {
    na.alpha <- ifelse(na.alpha == 0, 0.001, na.alpha)
    if (length(alpha) != nrow(x)) alpha <- rep(alpha, nrow(x))
    alpha[is.na(x[[zcol]])] <- na.alpha
    if (length(alpha.regions) != nrow(x))
      alpha.regions <- rep(alpha.regions, nrow(x))
    alpha.regions[is.na(x[[zcol]])] <- na.alpha
  }

  # Add appropriate layers based on geometry type
  # TODO: Add support for "gc" and "rs"
  m <- switch(
    geom_type,
    "pl" = mapgl_pl(m, col.regions, alpha.regions, color, lwd, alpha),
    "ln" = mapgl_ln(m, color, lwd, alpha),
    "pt" = mapgl_pt(m, alpha.regions, col.regions, color, lwd, cex)
  )

  # Add navigation control
  m <- mapgl::add_navigation_control(m)

  return(m)
}

# Helper functions for each geometry type
mapgl_pl <- function(m, col.regions, alpha.regions, color, lwd, alpha) {
  mapgl::add_fill_layer(
    map = m,
    id = "data_fill",
    source = "data",
    fill_color = col.regions,
    fill_opacity = alpha.regions
  ) |>
    mapgl::add_line_layer(
      id = "data_line",
      source = "data",
      line_color = color,
      line_width = lwd,
      line_opacity = alpha
    )
}

mapgl_ln <- function(m, color, lwd, alpha) {
  mapgl::add_line_layer(
    map = m,
    id = "data_line",
    source = "data",
    line_color = color,
    line_width = lwd,
    line_opacity = alpha
  )
}

mapgl_pt <- function(m, alpha.regions, col.regions, color, lwd, cex) {
  mapgl::add_circle_layer(
    map = m,
    id = "data_point",
    source = "data",
    circle_opacity = alpha.regions,
    circle_color = col.regions,
    circle_stroke_color = color,
    circle_stroke_width = lwd,
    circle_radius = cex
  )
}
