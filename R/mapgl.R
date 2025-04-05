mapgl_sf <- function(
  x,
  map = NULL,
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
  # Initialize map centered on data if no map is provided
  if (is.null(map)) {
    m <- mapgl::maplibre(bounds = x)
  } else {
    m <- map
  }

  # Set source ID based on layer name
  source_id <- if (!is.null(layer.name)) layer.name else "data"

  # Add source with unique ID based on layer name
  m <- mapgl::add_source(m, source_id, x)

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
    "pl" = mapgl_pl(
      m,
      source_id,
      col.regions,
      alpha.regions,
      color,
      lwd,
      alpha
    ),
    "ln" = mapgl_ln(
      m,
      source_id,
      color,
      lwd,
      alpha
    ),
    "pt" = mapgl_pt(
      m,
      source_id,
      alpha.regions,
      col.regions,
      color,
      lwd,
      cex
    ),
    m
  ) # Default case returns map unchanged

  # Add navigation control if this is a new map
  if (is.null(map)) {
    m <- mapgl::add_navigation_control(m)
  }

  # Create a mapview object to support the + operator
  # Store the original object with its name
  if (is.null(layer.name)) {
    layer.name <- deparse(substitute(x, env = parent.frame()))
  }
  out_obj <- list(x)
  names(out_obj) <- layer.name

  # Create a mapview object with the proper structure
  out <- new("mapview", object = out_obj, map = m)
  return(out)
}

# Implementation of the + operator for mapgl platform
# This function combines two mapview objects by extracting and merging their maplibre calls
mapgl_plus <- function(e1, e2) {
  # Get the map from the first mapview object
  m <- e1@map

  # Extract the second object's source and add it to the map
  for (name in names(e2@object)) {
    source_id <- name
    x <- e2@object[[name]]

    # Add the source from the second object
    m <- mapgl::add_source(m, source_id, x)

    # Determine geometry type
    geom_type <- getGeometryType(x)

    # Use the same styling approach as in mapgl_sf for consistency

    # FIXME:
    # How to inherit settings from mapview objects?
    color <- standardColor(x)
    col.regions <- standardColRegions(x)
    alpha <- 0.9
    alpha.regions <- regionOpacity(x)
    lwd <- 1
    cex <- 6

    # Add appropriate layers based on geometry type using switch
    m <- switch(
      geom_type,
      "pl" = mapgl_pl(
        m,
        source_id,
        col.regions,
        alpha.regions,
        color,
        lwd,
        alpha
      ),
      "ln" = mapgl_ln(m, source_id, color, lwd, alpha),
      "pt" = mapgl_pt(
        m,
        source_id,
        alpha.regions,
        col.regions,
        color,
        lwd,
        cex
      ),
      m
    ) # Default case returns map unchanged
  }

  # Combine the objects
  out_obj <- append(e1@object, e2@object)

  # Create a new mapview object with combined objects and updated map
  out <- new("mapview", object = out_obj, map = m)
  return(out)
}

# Helper functions for each geometry type
mapgl_pl <-
  function(
    m,
    source_id,
    col.regions,
    alpha.regions,
    color,
    lwd,
    alpha
  ) {
    m |>
      mapgl::add_fill_layer(
        id = paste0(source_id, "_fill"),
        source = source_id,
        fill_color = col.regions,
        fill_opacity = alpha.regions
      ) |>
      mapgl::add_line_layer(
        id = paste0(source_id, "_line"),
        source = source_id,
        line_color = color,
        line_width = lwd,
        line_opacity = alpha
      )
  }

mapgl_ln <-
  function(
    m,
    source_id,
    color,
    lwd,
    alpha
  ) {
    m |>
      mapgl::add_line_layer(
        id = paste0(source_id, "_line"),
        source = source_id,
        line_color = color,
        line_width = lwd,
        line_opacity = alpha
      )
  }

mapgl_pt <-
  function(
    m,
    source_id,
    alpha.regions,
    col.regions,
    color,
    lwd,
    cex
  ) {
    m |>
      mapgl::add_circle_layer(
        id = paste0(source_id, "_point"),
        source = source_id,
        circle_opacity = alpha.regions,
        circle_color = col.regions,
        circle_stroke_color = color,
        circle_stroke_width = lwd,
        circle_radius = cex
      )
  }
