#' mapview version of leaflet::colorNumeric
#'
#' @param x Spatial* or Raster* object
#' @param zcol the column to be colored
#' @param colors color vector to be used for coloring the levels
#' specified in at
#' @param at numeric vector giving the breakpoints for the colors
#' @param na.color the color for NA values.
#' @param ... additional arguments passed on to \code{\link{level.colors}}
#'
#' @author
#' Tim Appelhans
#'
#' @seealso
#' \code{\link{level.colors}}
#'
#' @name mapviewColors
#' @export mapviewColors
#' @aliases mapviewColors
mapviewColors <- function(x,
                          zcol = NULL,
                          colors = mapviewGetOption("vector.palette"),
                          at = NULL,
                          na.color = mapviewGetOption("na.color"),
                          ...) {

  stnd_col <- "#6666ff"

  if (typeof(x) == "list") {
    if (is.function(colors)) colors <- colors(length(x))
    return(col2Hex(colors))
  }

  if (inherits(x, "Spatial") && is.null(zcol)) {
    if (is.function(colors)) colors <- stnd_col #colors(1)
    return(col2Hex(colors))
  }

  if (!isMultiFeature(x)) {
    colors <- stnd_col
    return(col2Hex(colors))
  }

  if (isMultiFeature(x) && is.null(zcol)) {
    if (is.function(colors)) colors <- stnd_col #colors(1)
    return(col2Hex(colors))
  }

  if (inherits(x, "Spatial") && !is.null(zcol)) x <- x@data[, zcol]

  if (is.character(x)) x <- factor(x)
  x <- as.numeric(x)

  if (length(unique(x)) == 1) {
    if (is.function(colors)) colors <- colors(1)
    return(col2Hex(colors[1]))
  } else if (is.null(at)) {
    at <- lattice::do.breaks(range(x, na.rm = TRUE),
                             length(unique(x)))
    cols <- lattice::level.colors(x,
                                  at = at,
                                  col.regions = colors,
                                  ...)
    cols[is.na(cols)] <- na.color
    return(col2Hex(cols))
  } else {
    cols <- lattice::level.colors(x,
                                  at = at,
                                  col.regions = colors,
                                  ...)
    cols[is.na(cols)] <- na.color
    return(col2Hex(cols))
  }

  # attributes(f) <- list(colorType = "bin",
  #                       colorArgs = list(bins = at,
  #                                        na.color = na.color))
}


## raster colors
rasterColors <- function(col.regions,
                         at,
                         na.color) {

  f <- function(x) {

    cols <- lattice::level.colors(x,
                                  at = at,
                                  col.regions = col.regions)
    #cols <- col2Hex(cols)
    cols[is.na(cols)] <- na.color

    return(col2Hex(cols, alpha = TRUE))

  }

  attributes(f) <- list(colorType = "bin",
                        colorArgs = list(bins = at,
                                         na.color = na.color))

  return(f)

}


## hex color conversion
col2Hex <- function(col, alpha = FALSE) {

  mat <- grDevices::col2rgb(col, alpha = TRUE)
  if (alpha) {
    hx <- grDevices::rgb(mat[1, ]/255, mat[2, ]/255,
                         mat[3, ]/255, mat[4, ]/255)
  } else {
    hx <- grDevices::rgb(mat[1, ]/255, mat[2, ]/255, mat[3, ]/255)
  }
  return(hx)

}


## vector colors
vectorColors <- function(x, # a sp or sf object
                         zcol = NULL,
                         colors = mapviewGetOption("vector.palette"),
                         at = NULL,
                         na.color = mapviewGetOption("na.color"),
                         ...) {

  stnd_col <- standardColor(x) #"#333333"

  if (getGeometryType(x) == "ln") {
    if (!is.null(zcol)) {
      if (!inherits(x[[zcol]], c("numeric", "integer")) & !is.null(at)) {
        warning("ignoring 'at' which is only supported for numeric/integer values")
        at <- NULL
      }
      col <- zcolColors(x[[zcol]],
                        colors = colors,
                        at = at,
                        na.color = na.color,
                        ...)
    } else if (is.null(zcol) & is.function(colors)) {
      col <- standardColor(x)
    } else col <- colors
  } else if (is.function(colors)) {
    col <- stnd_col
  } else col <- colors

  return(col2Hex(col))

}


vectorColRegions <- function(x, # a sp or sf object
                             zcol = NULL,
                             col.regions = mapviewGetOption("vector.palette"),
                             at = NULL,
                             na.color = mapviewGetOption("na.color"),
                             ...) {

  stnd_col <- standardColRegions(x) #"#aaaaff" #"#CCCCCC"

  if (!is.null(zcol)) {
    if (!inherits(x[[zcol]], c("numeric", "integer")) & !is.null(at)) {
      warning("ignoring 'at' which is only supported for numeric values")
      at <- NULL
    }
    col <- zcolColors(x[[zcol]],
                      colors = col.regions,
                      at = at,
                      na.color = na.color,
                      ...)
  } else if (is.null(zcol) & is.function(col.regions)) {
    col <- stnd_col
  } else col <- col.regions

  return(col2Hex(col))

}

### if zcol is set, we use this function to produce colors according to
### levels of zcol or as provided by at
zcolColors <- function(x, # a vector, not a sp or sf object
                       colors = mapviewGetOption("vector.palette"),
                       at = NULL,
                       na.color = mapviewGetOption("na.color"),
                       return.sorted = FALSE,
                       ...) {

  if (!is.function(colors) & inherits(colors, "character")) {
    colors <- grDevices::colorRampPalette(colors)
  }

  if (is.character(x)) x <- as.factor(x)
  x <- as.numeric(x)

  if (length(unique(x)) == 1) {
    cols <- "#6666ff"
  } else {
    if (is.null(at)) {
      at <- lattice::do.breaks(range(x, na.rm = TRUE),
                               length(unique(x)))
    }

    cols <- lattice::level.colors(x,
                                  at = at,
                                  col.regions = colors,
                                  ...)
    if (return.sorted) cols <- cols[order(x)]

    cols[is.na(cols)] <- na.color
  }
  return(col2Hex(cols))

}


standardColor <- function(x) {
  switch(getGeometryType(x),
         "pt" = "#333333", #"#66b3ff",
         "ln" = "#6666ff", #"#66b3ff",
         "pl" = "#333333",
         "gc" = "#333333") #"#66b3ff")
}


standardColRegions <- function(x) {
  switch(getGeometryType(x),
         "pt" = "#6666ff", #"#66b3ff",
         "ln" = "#6666ff", #"#66b3ff",
         "pl" = "#6666ff",
         "gc" = "#6666ff") #"#66b3ff")
}


# from https://en.wikipedia.org/wiki/Relative_luminance
luminence <- function(color) {
  rgbcols <- as.numeric(grDevices::col2rgb(color))
  (0.2126 * rgbcols[1] + 0.7152 * rgbcols[2] + 0.0722 * rgbcols[3]) / 255
}
